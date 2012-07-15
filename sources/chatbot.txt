=============================
 Haskellで人工無脳をつくろう
=============================

はじめに
========

Haskellで人工無脳をつくると面白そうなのでやってみることにしました。Twitterのボットくらいを目指します。

Haskell開発環境を用意しよう
===========================

Haskell Platformのインストール
------------------------------

\ `Haskell Platform <http://hackage.haskell.org/platform/>`_\ から自分
のOSに対応するものをダウンロードしてインストールすると、\ `一通りの開発環境 <http://lambda.haskell.org/platform/doc/current/frames.html>`_\ 
が揃います。

cabalを使う
-----------

cabalは\ `Hackage <http://hackage.haskell.org/packages/hackage.html>`_\ のパッケージをインストールするためのツールです。
これはいわゆるperlのcpanやpythonのeasy_installにあたるものなので、早めに覚えるべきコマンドです。先人の知恵を上手に取り入れて
素早く開発するのはHaskellでも一緒です。

Haskell Platformをインストールすれば使えるようになっているはずなので、
早速コマンド入力用のユーザーインターフェースライブラリを導入してみます

.. code-block:: sh

   $ cabal install readline

幾つかのログ情報が端末に出力されて、インストールは終了すると思います。簡単ですね。

.. note::

   osxユーザーでreadlineをhomebrew経由でインストールしている場合は
   
   cabal install readline \
   --configure-option=--with-readline-libraries="/usr/local/Cellar/readline/6.2.2/lib" \
   --configure-option=--with-readline-includes="/usr/local/Cellar/readline/6.2.2/include"
   
   でインストールできるはずです。readlineのバージョンは利用しているものに適宜読み替えてください。

人工無脳の名前を考えよう
========================

まずは人工無能の名前を考えなければいけませんよね。
なんといっても、いい名前はモチベーションに必須ですからね。

というわけであらかじめ考えておきました。

命名「モナポ(monapo)」

- Haskellと言えばアレ
- 地元のB級グルメ「つけナポリタン」のご当地キャラ
- 地元の選挙区出身の原発問題担当大臣問題から

なかなかいい名前です。

オウム返しさせる
================

最初は人工無能の基本であるオウム返しをさせてみます。ちなみにうちの三歳
の息子も言ったことを繰り返すの、でちょっとめんどくさいと思う時がありま
すが、ヒトの会話の発達段階には必要なイベントなんでしょう。

.. code-block:: haskell

    import System.Console.Readline
    
    main :: IO ()
    main = loop 
      where loop = do
              maybeLine <- readline "> "
              case maybeLine of 
                Nothing -> return ()
                Just "quit" -> return ()
                Just line -> do
                        putStrLn $ "monapo> " ++ line
                        loop

上記のコードをmonapo.hsという名前で保存したら早速コンパイルしてみましょう

.. code-block:: sh

   $ ghc --make monapo
   [1 of 1] Compiling Main             ( monapo.hs, monapo.o )
   Linking monapo ...

無事コンパイルできたら、早速実行してみましょう。あいさつからはじめて、
とりあえず近所の民主党候補のポスターで覚えていたものをオウム返しさせて
みます。

.. code-block:: sh

   $ ./monapo 
   > こんにちわ
   monapo> こんにちわ
   > ごうしでごー
   monapo> ごうしでごー
   > たむけん参上！
   monapo> たむけん参上！


コードの説明
------------

まずはモジュールをインポートします。

.. code-block:: haskell

    import System.Console.Readline

続いてmainの定義を書きます。

- mainはloopっていう関数です
- loopっていう関数はreadlineで入力を受け付けて、入力が

 - 何もない場合と"quit"という文字列の場合はreturnする
 - それ以外の場合はオウム返ししてloop関数を実行する

.. code-block:: haskell

    main :: IO ()
    main = loop 
      where loop = do
              maybeLine <- readline "> "
              case maybeLine of 
                Nothing -> return ()
                Just "quit" -> return ()
                Just line -> do
                        putStrLn $ "monapo> " ++ line
                        loop

ポイントはdoです。do構文を使うと関数が順番に実行されるようになります。
よくある手続き型のスクリプト言語の感覚で書けるようになります。それがdo
です。

.. note::

   doはmoco'sキッチンにおけるオリーブオイルのようなものだと覚えておけば
   安心です。怖がらずなんにでもかけてみよう!(※ただしメインに限る)

下の例だとputStrLnした後にloopを実行してます。

.. code-block:: haskell

   Just line -> do
          putStrLn $ "monapo> " ++ line
          loop

ランダムに応答を返す
====================

オウム返しはうまくいったので、続いて予め決められた返事のリストからラン
ダムに一つ選んで返すようにしてみましょう。

本物のHaskellerはHoogleを使う
-----------------------------

コードの戦略としては応答候補のリストを用意して、これからランダムに一つ
選択したいわけですが、Haskellでランダムを扱うにはどうすればいいのでしょう?

.. code-block:: haskell
   :emphasize-lines: 16

    import System.Console.Readline
    
    responses = [
     "ジョジョ立ちしてみて",
     "こんにちわ",
     "すごいHaskellたのしく学んでますか？"]
    
    main :: IO ()
    main = loop
      where loop = do
              maybeLine <- readline "> "
              case maybeLine of
                Nothing -> return ()
                Just "quit" -> return ()
                Just line -> do
                     putStrLn $ "monapo> " -- responseをどうやって選択するのか?
                     loop

そんな時は\ `Hoogle <http://www.haskell.org/hoogle/>`_\ で検索してみま
しょう。randomというキーワードで検索すると\ `System.Random <http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html>`_\ 
がヒットするはずです。これは求めるものに近そうなのできちんと見ていきます。

.. note::

   cabal install でhoogleをインストールするとCUIで検索できるようになって便利なので入れておきましょう

randomパッケージの中から出力の型がIO [何か]というものを探します。なぜIOがついているものを探すかというとmain関数の型の定義が

.. code-block:: haskell

   main :: IO ()

となっているからです。\ **IOというラベルの張っている型はIOというラベル
の張っている型を出力する関数じゃないとつなげることができない**\ と覚え
ておけばいいでしょう。そんな感じで見ていくとgetStdRandomというものが見
つかりました。確認のために型を見てみると

.. code-block:: haskell

   getStdRandom :: (StdGen -> (a, StdGen)) -> IO a

となっています。StdGenとは一体何だ?理解するのに大変な予感がするんだ
が、、、、と気になるところですが、すぐ後ろに簡単な使い方が載っているの
で見てみると、

.. code-block:: haskell

   rollDice :: IO Int
   rollDice = getStdRandom (randomR (1,6))

ズバリな感じで、指定した範囲の数字をランダムに出力する関数みたいです。
これをちょっとモディファイすれば使えそうです。というわけで結局StdGenを
深追いしなくて良くなりましたね。あとはうまく貼りあわせればいいだけなの
で出来上がったコードを載せておきます。

.. code-block:: haskell

    import System.Console.Readline
    import System.Random
    
    randomNumGen :: IO Int
    randomNumGen = getStdRandom (randomR (0, (length response)-1))
    
    response = [
     "ジョジョ立ちしてみて",
     "こんにちわ",
     "すごいHaskellたのしく学んでますか？"]
    
    main :: IO ()
    main = loop
      where loop = do
              maybeLine <- readline "> "
              case maybeLine of
                Nothing -> return ()
                Just "quit" -> return ()
                Just line -> do
                     index <- randomNumGen
                     putStrLn $ "monapo> " ++ response !! index
                     loop

コードの説明
------------

responseでリストを定義してrandomNumGenでリストのインデックスの整数値が
ランダムに選ばれるようにしています。(関数の型がIO Intであることに気がつ
いてください)。あとはmain関数中で、ランダムに選ばれたindex値を用いて
responseの中から応答すべき文字列を選択しています。

.. code-block:: haskell

   index <- randomNumGen
   putStrLn $ "monapo> " ++ response !! index

感情モデルの作成
================

全体の流れ
----------

応答をランダムに返す人工無能ができたところで、応答の選び方をちょっと工
夫したくなりますよね?

例えば整数値を

-  1:ハッピー (両手で大きくハートを描いた後、手をハートの形に組んで光波を放ち、敵に直撃させて浄化するテンション)
-  0:ノーマル 
- -1:ネガティブ (白紙の未来を黒く塗りつぶす感じで)

という心理状態に見立てて、それぞれの値に応じたレスポンスを返すようにするのはどうでしょう?

先ほどのresponseに関しては

.. code-block:: haskell

    response = [
     (1, "ピースソーグーーッド！"),
     (1, "ウルトラハッピー"),
     (0, "こんにちわ"),
     (0, "すごいHaskellたのしく学んでますか？"),
     (-1, "お前それサバンナでも同じ事言えんの?"),
     (-1, "小町とHagexみすぎだろ")]

といった感じの定義を用意しておいて、(1,0,-1)の応答を返すようにfilter関
数でフィルターすればいいですね。

.. code-block:: haskell

    map snd $ filter (\s -> fst s == 1) response -- タプルの最初が1である応答のリストを返す

こんな感じで選ばれた応答リストをランダムに返せばよさそうです。

ロボット生命体的なアイテムの登場です
------------------------------------

続いて、monapoに心理状態を覚えさせることを考えます。ほめられたら1増やし
、けなされたら1減らすことにします。

IO ()にはStateTっていうプラグインみたいな感覚で付けられる型があるため、
それを使うとなぜか状態が扱えます。TはTransformerの頭文字で、ロボット生
命体のアレと一緒です。彼らも合体してより強くなるのでそれのイメージで理
解しておけば大丈夫です。

StateTを利用すると、doのなかでset,putが使えるようになります。これはま
るでデータベースのAPIを操作している感覚でデータの出し入れが行えるように
なるので、なんというかお手軽です。

他に、心理状態がIntなのはコードリーディング的に問題あるだろうという私の
特別なはからいによりEmotionという別名を付けておきました。

.. code-block:: haskell

    type Emotion = Int
    
    happ :: StateT Emotion IO Int
    happ = do
      em <- get
      if em+1 > 1 then do put 1 >> return 1 
      else do put (em+1) >> return (em+1)
    
    sad :: StateT Emotion IO Int
    sad = do
      em <- get
      if (em-1) < (-1) then do put (-1) >> return (-1) 
      else do put (em-1) >> return (em-1)
    
    keep :: StateT Emotion IO Int
    keep = do 
      em <- get
      return em

ここで、happは幸せ関数、sadは不幸せ関数、keepはkeep calmです。大したこ
とやってないので説明は端折ります。

ランダムに選択する部分もindexを返すんじゃなく直接応答を選択するように
ちょっと変えておきます

.. code-block:: haskell

    randomNumGen :: Int -> IO Int
    randomNumGen n = getStdRandom (randomR (0, n-1))
    
    choiceResponse ::  [String]-> IO String
    choiceResponse cs = do
      index <- randomNumGen $ length cs
      return (cs !! index)


ちょっとめんどくさいのはmainですが、もとのコードにStateT対応させるため
にliftとliftIOをかさねているだけです。入力でfacebookライクにいいね!されると幸
福度が増して、逆にダメ出しされると幸福度が下がるようにしてみました。

.. code-block:: haskell

    code :: StateT Emotion IO ()
    code = do
      loop
          where loop = do
                  maybeLine <- lift $ readline "> "
                  case maybeLine of
                    Nothing -> do lift $ return ()
                    Just "quit" -> do lift $ return ()
                    Just line -> do
                         case line of
                           "いいね" -> do
                                     em <- happ
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                           "だめだね" -> do
                                     em <- sad
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                           otherwise -> do
                                     em <- keep
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                         loop
    
    main :: IO ()
    main = runStateT code 0 >> return ()

全コード
--------

全部のコードを載せておきます。

.. code-block:: haskell

    import Control.Monad.State
    import System.Console.Readline
    import System.Random
    
    randomNumGen :: Int -> IO Int
    randomNumGen n = getStdRandom (randomR (0, n-1))
    
    choiceResponse ::  [String]-> IO String
    choiceResponse cs = do
      index <- randomNumGen $ length cs
      return (cs !! index)
    
    responses = [
     (1, "ピースソーグーーッド！"),
     (1, "ウルトラハッピー"),
     (0, "こんにちわ"),
     (0, "すごいHaskellたのしく学んでますか？"),
     (-1, "お前それサバンナでも同じ事言えんの?"),
     (-1, "小町とHagexみすぎだろ")]
    
    type Emotion = Int
    
    happ :: StateT Emotion IO Int
    happ = do
      em <- get
      if em+1 > 1 then do put 1 >> return 1 
      else do put (em+1) >> return (em+1)
    
    sad :: StateT Emotion IO Int
    sad = do
      em <- get
      if (em-1) < (-1) then do put (-1) >> return (-1) 
      else do put (em-1) >> return (em-1)
    
    keep :: StateT Emotion IO Int
    keep = do 
      em <- get
      return em
    
    code :: StateT Emotion IO ()
    code = do
      loop
          where loop = do
                  maybeLine <- lift $ readline "> "
                  case maybeLine of
                    Nothing -> do lift $ return ()
                    Just "quit" -> do lift $ return ()
                    Just line -> do
                         case line of
                           "いいね" -> do
                                     em <- happ
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                           "だめだね" -> do
                                     em <- sad
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                           otherwise -> do
                                     em <- keep
                                     choiced <- lift $ choiceResponse $ map snd $ filter (\s -> fst s == em) responses
                                     liftIO $ putStrLn $ "monapo> " ++ choiced
                         loop
    
    main :: IO ()
    main = runStateT code 0 >> return ()

動かしてみよう
--------------

せっかくつくったので少し動かしてみましょう。

.. code-block:: sh

    $ ./monapoS
    > こんにちわ
    monapo> こんにちわ
    > いいね
    monapo> ピースソーグーーッド！
    > だめだね
    monapo> すごいHaskellたのしく学んでますか？
    > だめだね
    monapo> お前それサバンナでも同じ事言えんの?

