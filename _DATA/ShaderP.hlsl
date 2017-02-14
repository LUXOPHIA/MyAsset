//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

struct TColorVec3
{
    float3 R;
    float3 G;
    float3 B;
};

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

static const float Pi = 3.141592653589793;

static const float Pi2 = Pi * 2.0;

static const float P2i = Pi / 2.0;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【設定】

SamplerState _SamplerState {};

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

float Pow2( float X_ )
{
    return X_ * X_;
}

float Roo2( float X_ )
{
    return sqrt( X_ );
}

float2 VectorToSky( float3 Vector_ )
{
    float2 _Result;

    _Result.x = ( Pi - atan2( -Vector_.z, -Vector_.x ) ) / Pi2;
    _Result.y =        acos ( -Vector_.y             )   / Pi ;

    return _Result;
}

float Fresnel( float3 EyeVec_, float3 NorVec_, float RefI_ )
{
    float N = Pow2( RefI_ );
    float C = dot( EyeVec_, NorVec_ );
    float G = sqrt( N + Pow2( C ) - 1 );
    float NC = N * C;
    return ( Pow2( (  C - G ) / (  C + G ) )
           + Pow2( ( NC - G ) / ( NC + G ) ) ) / 2;

    /* 近似
    float R = pow( ( RefI_ - 1 ) / ( RefI_ + 1 ), 2 );
    float C = dot( EyeVec_, NorVec_ );
    return R + ( 1 - R ) * pow( 1 - C, 5 );
    */
}

float4 Fresnel4( float3 EyeVec_, float3 NorVec_, float4 RefI_ )
{
    float4 Result;

    Result.r = Fresnel( EyeVec_, NorVec_, RefI_.r );
    Result.g = Fresnel( EyeVec_, NorVec_, RefI_.g );
    Result.b = Fresnel( EyeVec_, NorVec_, RefI_.b );
    Result.a = Fresnel( EyeVec_, NorVec_, RefI_.a );

    return Result;
}

TColorVec3 Refract3( float3 EyeVec_, float NorVec_, float4 RefI_ )
{
    TColorVec3 Result;

    Result.R = normalize( refract( -EyeVec_, NorVec_, 1/RefI_.r ) );
    Result.G = normalize( refract( -EyeVec_, NorVec_, 1/RefI_.g ) );
    Result.B = normalize( refract( -EyeVec_, NorVec_, 1/RefI_.b ) );

    return Result;
}

//##############################################################################

struct TSenderP               //フラグメントの変数型
{
    float4 Scr :SV_Position;  //位置（スクリーン）
    float4 Pos :TEXCOORD0  ;  //位置（グローバル）
    float4 Tan :TANGENT    ;  //接線（グローバル）
    float4 Bin :BINORMAL   ;  //従法線（グローバル）
    float4 Nor :NORMAL     ;  //法線（グローバル）
    float4 Tex :TEXCOORD1  ;  //テクスチャ座標
};

struct TResultP               //ピクセルの変数型
{
    float4 Col :SV_Target  ;  //色
};

////////////////////////////////////////////////////////////////////////////////

float4 Sample3( TColorVec3 Vec_ )
{
    float4 Result;

    Result.r = _EnviImage.Sample( _SamplerState, VectorToSky( Vec_.R ) ).r;
    Result.g = _EnviImage.Sample( _SamplerState, VectorToSky( Vec_.G ) ).g;
    Result.b = _EnviImage.Sample( _SamplerState, VectorToSky( Vec_.B ) ).b;

    return Result;
}

////////////////////////////////////////////////////////////////////////////////

TResultP MainP( TSenderP _Sender )
{
    TResultP _Result;

    float3 N = normalize( _Sender.Nor.xyz );                                    //表面法線（グローバル）
    float3 T = normalize( _Sender.Tan.xyz );                                    //表面接線（グローバル）
    float3 B = normalize( _Sender.Bin.xyz );                                    //表面従法線（グローバル）
    float3 L = -_Light.Dir.xyz;                                                 //光線方向（グローバル）
    float3 V = normalize( _EyePos.xyz - _Sender.Pos.xyz );                      //視線方向（グローバル）
    float3 H = normalize( L + V );                                              //ハーフベクトル

    //--------------------------------------------------------------------------

    float4 NP = _NormImage.Sample( _SamplerState, _Sender.Tex.xy );             //法線マップ色

    N = normalize( ( NP.r - 0.5 ) * 2 * T
                 + ( NP.g - 0.5 ) * 2 * B
                 +   NP.b             * N );

    //--------------------------------------------------------------------------

    float D = max( dot( N, L ), 0.0 );                                          //光線拡散反射率

    float4 DP = _DiffImage.Sample( _SamplerState, _Sender.Tex.xy );             //拡散反射率マップ色

    float4 CD = ( _AmbiLight + _Light.Col * D ) * ( _DiffRatio * DP );          //拡散反射光

    //--------------------------------------------------------------------------

    float S = pow( max( dot( N, H ), 0.0 ), _SpecShiny );                       //光線鏡面反射率

    float4 CS = ( _Light.Col * S ) * _SpecRatio;                                //光線鏡面反射光

    //--------------------------------------------------------------------------

    float3 E = normalize( reflect( -V, N ) );                                   //視線鏡面反射方向

    float4 CE = _EnviImage.Sample( _SamplerState, VectorToSky( E ) );           //環境鏡面反射光

    //--------------------------------------------------------------------------

    TColorVec3 A = Refract3( V, N, _RefrIndex );                                //視線鏡面屈折方向×RGB

    float4 CA = Sample3( A );                                                   //環境鏡面屈折光

    //--------------------------------------------------------------------------

    float4 CI = _EmisLight + ( CA - CD ) * _TranRatio + CD;                     //内部光

    float4 CO = CE + CS;                                                        //鏡面反射光

    float4 F = Fresnel4( V, N, _RefrIndex );                                    //視線鏡面反射率

    F = clamp( F, 0, 1 );

    _Result.Col = ( CO - CI ) * F + CI;

    //--------------------------------------------------------------------------

    _Result.Col.a = 1;

    _Result.Col = _Opacity * _Result.Col;

    return _Result;
}

//##############################################################################
