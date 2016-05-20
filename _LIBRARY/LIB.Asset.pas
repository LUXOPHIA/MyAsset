unit LIB.Asset;

interface //#################################################################### ■

uses System.Types, System.Classes, System.UITypes, System.Math.Vectors, System.Messaging,
     FMX.Graphics, FMX.Types3D, FMX.Controls3D, FMX.MaterialSources,
     LUX, LUX.FMX.Material;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

     TMyMaterial = class( TLuxMaterial )
     private
     protected
       _FMatrixMVP :TShaderVarMatrix3D;
       _FMatrixMV  :TShaderVarMatrix3D;
       _TIMatrixMV :TShaderVarMatrix3D;
       _Light      :TShaderVarLight;
       _EyePos     :TShaderVarVector3D;
       _Opacity    :TShaderVarSingle;
       _EmisLight  :TShaderVarColorF;
       _AmbiLight  :TShaderVarColorF;
       _DiffRatio  :TShaderVarColorF;
       _SpecRatio  :TShaderVarColorF;
       _SpecShiny  :TShaderVarSingle;
       _TranRatio  :TShaderVarColorF;
       _RefrIndex  :TShaderVarColorF;
       _DiffImage  :TShaderVarTexture;
       _NormImage  :TShaderVarTexture;
       _EnviImage  :TShaderVarTexture;
       ///// メソッド
       procedure DoApply( const Context_:TContext3D ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisLight :TShaderVarColorF  read _EmisLight;
       property AmbiLight :TShaderVarColorF  read _AmbiLight;
       property DiffRatio :TShaderVarColorF  read _DiffRatio;
       property SpecRatio :TShaderVarColorF  read _SpecRatio;
       property SpecShiny :TShaderVarSingle  read _SpecShiny;
       property TranRatio :TShaderVarColorF  read _TranRatio;
       property RefrIndex :TShaderVarColorF  read _RefrIndex;
       property DiffImage :TShaderVarTexture read _DiffImage;
       property NormImage :TShaderVarTexture read _NormImage;
       property EnviImage :TShaderVarTexture read _EnviImage;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

     TMyMaterialSource = class( TLuxMaterialSource<TMyMaterial> )
     private
     protected
       _ContextResetId :Integer;
       _DiffImage      :TBitmap;
       _NormImage      :TBitmap;
       _EnviImage      :TBitmap;
       ///// アクセス
       function GetEmisLight :TAlphaColorF;
       procedure SetEmisLight( const EmisLight_:TAlphaColorF );
       function GetAmbiLight :TAlphaColorF;
       procedure SetAmbiLight( const AmbiLight_:TAlphaColorF );
       function GetDiffRatio :TAlphaColorF;
       procedure SetDiffRatio( const DiffRatio_:TAlphaColorF );
       function GetSpecRatio :TAlphaColorF;
       procedure SetSpecRatio( const SpecRatio_:TAlphaColorF );
       function GetSpecShiny :Single;
       procedure SetSpecShiny( const SpecShiny_:Single );
       function GetTranRatio :TAlphaColorF;
       procedure SetTranRatio( const TranRatio_:TAlphaColorF );
       function GetRefrIndex :TAlphaColorF;
       procedure SetRefrIndex( const RefrIndex_:TAlphaColorF );
       procedure SetDiffImage( const DiffImage_:TBitmap );
       procedure SetNormImage( const NormImage_:TBitmap );
       procedure SetEnviImage( const EnviImage_:TBitmap );
       ///// メソッド
       procedure ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
       procedure DoDiffImageChanged( Sender_:TObject );
       procedure DoEnviImageChanged( Sender_:TObject );
       procedure DoNormImageChanged( Sender_:TObject );
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisLight :TAlphaColorF read GetEmisLight write SetEmisLight;
       property AmbiLight :TAlphaColorF read GetAmbiLight write SetAmbiLight;
       property DiffRatio :TAlphaColorF read GetDiffRatio write SetDiffRatio;
       property SpecRatio :TAlphaColorF read GetSpecRatio write SetSpecRatio;
       property SpecShiny :Single       read GetSpecShiny write SetSpecShiny;
       property TranRatio :TAlphaColorF read GetTranRatio write SetTranRatio;
       property RefrIndex :TAlphaColorF read GetRefrIndex write SetRefrIndex;
       property DiffImage :TBitmap      read   _DiffImage write SetDiffImage;
       property NormImage :TBitmap      read   _NormImage write SetNormImage;
       property EnviImage :TBitmap      read   _EnviImage write SetEnviImage;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyAsset

     TMyAsset = class( TControl3D )
     private
       ///// メソッド
       function XYtoI( const X_,Y_:Integer ) :Integer; inline;
       procedure MakeModel;
     protected
       _Geometry :TMeshData;
       _Material :TMyMaterialSource;
       _RadiusL  :Single;
       _RadiusS  :Single;
       _DivL     :Integer;
       _DivS     :Integer;
       ///// アクセス
       procedure SetRadiusL( const RadiusL_:Single );
       procedure SetRadiusS( const RadiusS_:Single );
       procedure SetDivL( const DivL_:Integer );
       procedure SetDivS( const DivS_:Integer );
       ///// メソッド
       procedure Render; override;
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property Geometry :TMeshData         read _Geometry                 ;
       property Material :TMyMaterialSource read _Material                 ;
       property DivL     :Integer           read _DivL     write SetDivL   ;
       property DivS     :Integer           read _DivS     write SetDivS   ;
       property RadiusL  :Single            read _RadiusL  write SetRadiusL;
       property RadiusS  :Single            read _RadiusS  write SetRadiusS;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function Torus( const RadiusL_,RadiusS_,TX_,TY_:Single ) :TMatrix3D;

implementation //############################################################### ■

uses System.SysUtils, System.RTLConsts, System.Math,
     LUX.D3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterial.DoApply( const Context_:TContext3D );
begin
     inherited;

     with Context_ do
     begin
          SetShaders( _ShaderV.Shader, _ShaderP.Shader );

          _FMatrixMVP.Value := CurrentModelViewProjectionMatrix;
          _FMatrixMV .Value := CurrentMatrix;
          _TIMatrixMV.Value := CurrentMatrix.Inverse.Transpose;
          _EyePos    .Value := CurrentCameraInvMatrix.M[ 3 ];
          _Opacity   .Value := CurrentOpacity;
          _Light     .Value := Lights[ 0 ];
     end;

     _ShaderV.SendVars( Context_ );
     _ShaderP.SendVars( Context_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterial.Create;
begin
     inherited;

     _FMatrixMVP := TShaderVarMatrix3D.Create( 'FMatrixMVP'  );
     _FMatrixMV  := TShaderVarMatrix3D.Create( 'FMatrixMV'   );
     _TIMatrixMV := TShaderVarMatrix3D.Create( 'IMatrixMV'   );
     _EyePos     := TShaderVarVector3D.Create( '_EyePos'     );
     _Opacity    := TShaderVarSingle  .Create( '_Opacity'    );
     _EmisLight  := TShaderVarColorF  .Create( '_EmisLight'  );
     _AmbiLight  := TShaderVarColorF  .Create( '_AmbiLight'  );
     _DiffRatio  := TShaderVarColorF  .Create( '_DiffRatio'  );
     _SpecRatio  := TShaderVarColorF  .Create( '_SpecRatio'  );
     _SpecShiny  := TShaderVarSingle  .Create( '_SpecShiny'  );
     _Light      := TShaderVarLight   .Create( '_Light'      );
     _DiffImage  := TShaderVarTexture .Create( '_DiffImage'  );
     _NormImage  := TShaderVarTexture .Create( '_NormImage'  );
     _EnviImage  := TShaderVarTexture .Create( '_EnviImage'  );
     _TranRatio  := TShaderVarColorF  .Create( '_TranRatio'  );
     _RefrIndex  := TShaderVarColorF  .Create( '_RefrIndex'  );

     _ShaderV.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV ];

     _ShaderP.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV,
                        _EyePos    ,
                        _Opacity   ,
                        _EmisLight ,
                        _AmbiLight ,
                        _DiffRatio ,
                        _SpecRatio ,
                        _SpecShiny ,
                        _Light     ,
                        _DiffImage ,
                        _NormImage ,
                        _EnviImage ,
                        _TranRatio ,
                        _RefrIndex  ];

     _EmisLight.Value := TAlphaColorF.Create( 0, 0, 0 );
     _AmbiLight.Value := TAlphaColorF.Create( 0.1, 0.1, 0.1 );
     _DiffRatio.Value := TAlphaColorF.Create( 1, 1, 1 );
     _SpecRatio.Value := TAlphaColorF.Create( 1, 1, 1 );
     _SpecShiny.Value := 30;
     _TranRatio.Value := TAlphaColorF.Create( 1, 1, 1 );
     _RefrIndex.Value := TAlphaColorF.Create( 2.417, 2.417, 2.417 );
end;

destructor TMyMaterial.Destroy;
begin
     _FMatrixMVP.Free;
     _FMatrixMV .Free;
     _TIMatrixMV.Free;
     _EyePos    .Free;
     _Opacity   .Free;
     _EmisLight .Free;
     _AmbiLight .Free;
     _DiffRatio .Free;
     _SpecRatio .Free;
     _SpecShiny .Free;
     _Light     .Free;
     _DiffImage .Free;
     _NormImage .Free;
     _EnviImage .Free;
     _TranRatio .Free;
     _RefrIndex .Free;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TMyMaterialSource.GetEmisLight :TAlphaColorF;
begin
     Result := _Material.EmisLight.Value;
end;

procedure TMyMaterialSource.SetEmisLight( const EmisLight_:TAlphaColorF );
begin
     _Material.EmisLight.Value := EmisLight_;
end;

function TMyMaterialSource.GetAmbiLight :TAlphaColorF;
begin
     Result := _Material.AmbiLight.Value;
end;

procedure TMyMaterialSource.SetAmbiLight( const AmbiLight_:TAlphaColorF );
begin
     _Material.AmbiLight.Value := AmbiLight_;
end;

function TMyMaterialSource.GetDiffRatio: TAlphaColorF;
begin
     Result := _Material.DiffRatio.Value;
end;

procedure TMyMaterialSource.SetDiffRatio( const DiffRatio_:TAlphaColorF );
begin
     _Material.DiffRatio.Value := DiffRatio_;
end;

function TMyMaterialSource.GetSpecRatio :TAlphaColorF;
begin
     Result := _Material.SpecRatio.Value;
end;

procedure TMyMaterialSource.SetSpecRatio( const SpecRatio_:TAlphaColorF );
begin
     _Material.SpecRatio.Value := SpecRatio_;
end;

function TMyMaterialSource.GetSpecShiny :Single;
begin
     Result := _Material.SpecShiny.Value;
end;

procedure TMyMaterialSource.SetSpecShiny( const SpecShiny_:Single );
begin
     _Material.SpecShiny.Value := SpecShiny_;
end;

procedure TMyMaterialSource.SetDiffImage( const DiffImage_:TBitmap );
begin
     _DiffImage.Assign( DiffImage_ );
end;

procedure TMyMaterialSource.SetNormImage( const NormImage_:TBitmap );
begin
     _NormImage.Assign( NormImage_ );
end;

procedure TMyMaterialSource.SetEnviImage( const EnviImage_:TBitmap );
begin
     _EnviImage.Assign( EnviImage_ );
end;

function TMyMaterialSource.GetTranRatio :TAlphaColorF;
begin
     Result := _Material.TranRatio.Value;
end;

procedure TMyMaterialSource.SetTranRatio( const TranRatio_:TAlphaColorF );
begin
     _Material.TranRatio.Value := TranRatio_;
end;

function TMyMaterialSource.GetRefrIndex :TAlphaColorF;
begin
     Result := _Material.RefrIndex.Value;
end;

procedure TMyMaterialSource.SetRefrIndex( const RefrIndex_:TAlphaColorF );
begin
     _Material.RefrIndex.Value := RefrIndex_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterialSource.ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
begin
     DoDiffImageChanged( Self );
     DoNormImageChanged( Self );
     DoEnviImageChanged( Self );
end;

procedure TMyMaterialSource.DoDiffImageChanged( Sender_:TObject );
begin
     if not _DiffImage.IsEmpty then _Material.DiffImage.Value := TTextureBitmap( _DiffImage ).Texture;
end;

procedure TMyMaterialSource.DoNormImageChanged( Sender_:TObject );
begin
     if not _NormImage.IsEmpty then _Material.NormImage.Value := TTextureBitmap( _NormImage ).Texture;
end;

procedure TMyMaterialSource.DoEnviImageChanged( Sender_:TObject );
begin
     if not _EnviImage.IsEmpty then _Material.EnviImage.Value := TTextureBitmap( _EnviImage ).Texture;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterialSource.Create( Owner_:TComponent );
begin
     inherited;

     _ContextResetId := TMessageManager.DefaultManager.SubscribeToMessage( TContextResetMessage, ContextResetHandler );

     _DiffImage := TTextureBitmap.Create;
     _NormImage := TTextureBitmap.Create;
     _EnviImage := TTextureBitmap.Create;

     _DiffImage.OnChange := DoDiffImageChanged;
     _NormImage.OnChange := DoNormImageChanged;
     _EnviImage.OnChange := DoEnviImageChanged;
end;

destructor TMyMaterialSource.Destroy;
begin
     FreeAndNil( _DiffImage );
     FreeAndNil( _NormImage );
     FreeAndNil( _EnviImage );

     TMessageManager.DefaultManager.Unsubscribe( TContextResetMessage, _ContextResetId );

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyAsset

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

function TMyAsset.XYtoI( const X_,Y_:Integer ) :Integer;
begin
     Result := ( _DivL + 1 ) * Y_ + X_;
end;

procedure TMyAsset.MakeModel;
var
   X ,Y, I :Integer;
   TX, TY :Single;
begin
     with _Geometry do
     begin
          with VertexBuffer do
          begin
               Length := ( _DivL + 1 ) * ( _DivS + 1 );

               for Y := 0 to _DivS do
               begin
                    TY := 1 - Y / _DivS;

                    for X := 0 to _DivL do
                    begin
                         TX := X / _DivL;

                         I := XYtoI( X, Y );

                         with Torus( _RadiusL, _RadiusS, TX, TY ) do
                         begin
                              Tangents [ I ] := TPoint3D( M[ 0 ] );
                              BiNormals[ I ] := TPoint3D( M[ 1 ] );
                              Normals  [ I ] := TPoint3D( M[ 2 ] );
                              Vertices [ I ] := TPoint3D( M[ 3 ] );
                         end;

                         TexCoord0[ I ] := TPointF.Create( TX, TY );
                    end;
               end;
          end;

          with IndexBuffer do
          begin
               Length := 3{Poin} * 2{Face} * _DivL * _DivS;

               I := 0;
               for Y := 0 to _DivS-1 do
               begin
                    for X := 0 to _DivL-1 do
                    begin
                         //    X0      X1
                         //  Y0┼───┼
                         //    │＼    │
                         //    │  ＼  │
                         //    │    ＼│
                         //  Y1┼───┼

                         Indices[ I ] := XYtoI( X  , Y   );  Inc( I );
                         Indices[ I ] := XYtoI( X+1, Y   );  Inc( I );
                         Indices[ I ] := XYtoI( X+1, Y+1 );  Inc( I );

                         Indices[ I ] := XYtoI( X+1, Y+1 );  Inc( I );
                         Indices[ I ] := XYtoI( X  , Y+1 );  Inc( I );
                         Indices[ I ] := XYtoI( X  , Y   );  Inc( I );
                    end;
               end;
          end;

          //CalcSmoothNormals( TCalculateNormalMethod.Fastest );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TMyAsset.SetRadiusL( const RadiusL_:Single );
begin
     _RadiusL := RadiusL_;  MakeModel;
end;

procedure TMyAsset.SetRadiusS( const RadiusS_:Single );
begin
     _RadiusS := RadiusS_;  MakeModel;
end;

procedure TMyAsset.SetDivL( const DivL_:Integer );
begin
     _DivL := DivL_;  MakeModel;
end;

procedure TMyAsset.SetDivS( const DivS_:Integer );
begin
     _DivS := DivS_;  MakeModel;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyAsset.Render;
begin
     Context.SetMatrix( AbsoluteMatrix );

     _Geometry.Render( Context, _Material.Material, AbsoluteOpacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyAsset.Create( Owner_:TComponent );
begin
     inherited;

     _Geometry := TMeshData.Create;

     _Material := TMyMaterialSource.Create( Self );

     HitTest  := False;

     _RadiusL := 1.0;
     _RadiusS := 0.5;

     _DivL    := 64;
     _DivS    := 64;

     MakeModel;
end;

destructor TMyAsset.Destroy;
begin
     _Geometry.Free;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function Torus( const RadiusL_,RadiusS_,TX_,TY_:Single ) :TMatrix3D;
var
   AX, AY, Xc, Xs, Yc, Ys :Single;
   CL, CS, P, EX, EY, EZ :TSingle3D;
begin
     AX := Pi2 * TX_;
     AY := Pi2 * TY_;

     Xc := Cos( AX );  Yc := Cos( AY );
     Xs := Sin( AX );  Ys := Sin( AY );

     CL.X := RadiusL_ * Xc;
     CL.Y := RadiusL_ * Xs;
     CL.Z := 0            ;

     CS.X := RadiusS_ * Yc;
     CS.Y := 0            ;
     CS.Z := RadiusS_ * Ys;

     with P do
     begin
          X := CL.X - CS.X * Xc;
          Y := CL.Y - CS.X * Xs;
          Z := CL.Z - CS.Z     ;
     end;

     with EX do
     begin
          X := -CL.Y;
          Y := +CL.X;
          Z :=  CL.Z;
     end;

     with EY do
     begin
          X := +CS.Z * Xc;
          Y := +CS.Z * Xs;
          Z := -CS.X     ;
     end;

     EZ := P - CL;

     with Result do
     begin
          M[ 0 ] := TVector3D.Create( EX.Unitor, 0 );
          M[ 1 ] := TVector3D.Create( EY.Unitor, 0 );
          M[ 2 ] := TVector3D.Create( EZ.Unitor, 0 );
          M[ 3 ] := TVector3D.Create( P        , 1 );
     end;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
