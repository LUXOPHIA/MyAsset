# MyAsset
[MyModel](https://bitbucket.org/LUXOPHIA/mymodel) と [MyMaterial](https://bitbucket.org/LUXOPHIA/mymaterial) の統合プロジェクト。

独自のシェーダを[HLSL](https://www.wikiwand.com/ja/High_Level_Shading_Language)で記述することにより、[フレネル反射](https://www.wikiwand.com/ja/%E3%83%95%E3%83%AC%E3%83%8D%E3%83%AB%E3%81%AE%E5%BC%8F)による鏡面反射/屈折の表現と、[法線マッピング](https://www.wikiwand.com/ja/%E6%B3%95%E7%B7%9A%E3%83%9E%E3%83%83%E3%83%94%E3%83%B3%E3%82%B0)による凸凹表現を実現。RGB毎に異なる屈折率を設定することで、簡易な[分散](https://www.wikiwand.com/ja/%E5%88%86%E6%95%A3_(%E5%85%89%E5%AD%A6))表現に対応できる他、屈折率を大きくすることで光の透過しない金属の質感も表現できる。

> **【図1】 実行画面**  
> [![MyAsset.png](https://bytebucket.org/LUXOPHIA/myasset/raw/8b9bc65ad4bd7c6fadefe0c2b1af22209ff6a626/--------/_SCREENSHOT/MyAsset.png)](https://youtu.be/4tx1NlBJRdc)