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


.. todo:: コードの説明

