=========================================================
 Haskellでスタックを利用した加減乗除の計算機を作ってみる
=========================================================

モナドはStateの理解から入ったほうが分かりやすいんじゃないかなぁと。よく
あるMaybeとかEitherから入るとStateモナドで激はまりしますよね?

僕はStateモナドから入ったほうがいいんじゃないかなーと思ったのでそんな題
材を考えてみました。自分はPerl,Pythonがフィールドなので、状態付き計算か
ら入ったほうがすんなり理解できたし、そういうヒトもいるかなと思って書い
てみました。

ちなみに文章のほとんどを\ `コミュニティf <http://com-f.net/>`_\ でもく
もくしながら書きました。あそこを静岡Haskellerの聖地にするべく頑張ってい
ます。


尚、特に参考になった(というかインスパイアされた)のが、

- `Haskell の State モナド (1) - 状態を模倣する <http://jutememo.blogspot.jp/2009/10/haskell-state-1.html>`_ 
- `すごいHaskellたのしく学ぼうの14.3 計算の状態の正体 <http://www.amazon.co.jp/dp/4274068854>`_ 
- `状態モナド遊び <http://d.hatena.ne.jp/kazu-yamamoto/20080604/1212573964>`_

なので、併せて読むといいです。先人の知恵に感謝しつつ。

スタックを実装しよう
====================


よいこのみんなは知ってる通り、データを後入れ先出し (LIFO: Last In
First Out; FILO: First In Last Out) の構造で保持するものなので、pushと
popの2つのメソッドを持っていればいいですね。

例えば、Pythonでスタックを実装する場合には破壊的操作(つまり再代入)が可
能なので、stackという変数を用意しておけばいい感じに使えます。

.. code-block:: python

    >>> stack = []
    >>> stack.append(1)
    >>> stack.append(2)
    >>> stack.append(3)
    >>> stack
    [1, 2, 3]
    >>> stack.pop()
    3
    >>> stack.pop()
    2
    >>> stack
    [1]

同じことをHaskellでやろうとすると

.. code-block:: haskell

    type Stack = [Integer]
    
    push :: Integer -> Stack -> Stack
    push c cs = c:cs
    
    pop :: Stack -> Integer
    pop (c:cs) = c

このように書けますが、popしてみると

.. code-block:: sh

   *Main> let stack = [3,2,1]
   *Main> pop stack
   3
   *Main> pop stack
   3
   *Main> pop stack
   3

haskellでは破壊的な操作ができないので、スタックから要素を取り出した後の
スタックも一緒に返さないとダメみたいです。

スタックを使って操作した結果と、その結果として変化したスタックを常に持
ちまわる必要があるので、haskellでのスタックの実装は次のようになりました。

.. code-block:: haskell

    type Stack = [Integer]
    
    push :: Integer -> Stack -> (Integer, Stack)
    push c cs = (c, c:cs)
    
    pop :: Stack -> (Integer, Stack)
    pop (c:cs) = (c,cs)

実際に使ってみます。

.. code-block:: haskell

     $ ghci MyStack.hs
     GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
     Loading package ghc-prim ... linking ... done.
     Loading package integer-gmp ... linking ... done.
     Loading package base ... linking ... done.
     [1 of 1] Compiling Main             ( MyStack.hs, interpreted )
     Ok, modules loaded: Main.
     *Main> let stack = []
     *Main> let (_, stack1) = push 1 stack
     *Main> let (_, stack2) = push 2 stack1
     *Main> let (_, stack3) = push 3 stack2
     *Main> stack3
     [3,2,1]
     *Main> let (r1, stack4) = pop stack3
     *Main> r1
     3
     *Main> stack4
     [2,1]
     *Main> let (r2, stack5) = pop stack4
     *Main> stack5
     [1]

毎回新しいスタックに名前をつけて、次の操作に渡さないといけないなんて超
めんどくさいですね。

しかし、我々がこのめんどくささから得た教訓はまさに、 **操作の結果得た新
しいスタックを次の操作に渡さないと次の操作は行えない** ということです。
そして逆に考えると、初期値を与えるとシーケンシャルに一気に流れるような
おおきな一つの流れを作っているのだとも言えます。

これはまったくもって **ぷよぷよの連鎖パターン** の習得に似ていますね。

.. todo:: ぷよぷよの連鎖パターンの図

上記のlet式の流れを見てみると、stackを取って操作をする(この場合はpush)
関数が見えてきます。

.. code-block:: haskell

   \stack -> push 1 $ stack

ラプラスの瞳で見るとこれが構成単位になりそうですね。それは後のほうで明
らかになることでしょう。

加算を実装してみる
==================

さて、スタックが実装できたところで加算を作ってみましょう。加算という操
作は

- スタックから2つpop
- 値を足す
- その値をpushする

であり、pop2つとpush1つが順番に行われます。

.. code-block:: haskell

    add :: Stack -> (Integer, Stack)
    add stack = 
        let
            (i1, stack1) = pop stack
            (i2, stack2) = pop stack1
        in
	    push (i1+i2) stack2

これはうまく動きます

.. code-block:: sh

    *Main> let stack = [3,2,1]
    *Main> let (_, stack') = add stack
    *Main> stack'
    [5,1]

続いて減乗除も実装したいところですが、共通のパターンをくくりだしてしま
います。addも再定義しておきます。

.. code-block:: haskell

    calc :: (Integer -> Integer -> Integer) -> Stack -> (Integer, Stack)
    calc op stack = 
        let
            (i1, stack1) = pop stack
            (i2, stack2) = pop stack1
        in
	    push (op i1 i2) stack2
    
    add = calc (+)
    sub = calc (-)
    mul = calc (*)
    dvv = calc div

計算結果が正しい場合にはスタックの中には1つの要素しか残っていないので、
その評価をするevalという関数を定義しておきます。(でもこれは微妙に使わな
いかもしれません)

.. code-block:: haskell

    eval :: Stack -> Integer
    eval [r] = r
    eval cs = error $ "Invalid Stack " ++ (show cs)

それでは適当に計算してみましょう。

共通のパターンを見つける
========================

先のcalc関数において、stackの状態をstack1,stack2と使いもしないのに名前
をつけるのは無駄だと思うんですよね。それに、この計算の流れ自体がパター
ンにできると思いませんか?

calcの引数を右に移行してラムダ式にします。

.. code-block:: haskell

    calc :: (Integer -> Integer -> Integer) -> Stack -> ((Integer), Stack)
    calc op = \stack ->  
        let
            (i1, stack1) = pop stack
            (i2, stack2) = pop stack1
            (_, stack3)  = push (op i1 i2) stack2
        in
          ((op i1 i2), stack3)

続いて、ぷよぷよの連鎖パターンを思い浮かべると

.. code-block:: haskell

    \stack  -> (i1, stack1)
    \stack1 -> (i2, stack2)
    \stack2 -> (i3, stack3)

がつながっているのが見えてきます。これが面白いのが、\s -> (i, s)を連鎖
して大きくしたモノの型もまた\s -> (i, s)の形になっているところですね。
(calcの関数定義をみるべし)

2つの\s -> (i, s)という型をうまくつないでより大きな\s -> (i, s)という型
を作れれば、これは無限に連鎖できることになります。そういう関数と関数を
結合する関数ををbindと定義し作ってみましょう。

ただし一つ注意しないといけないことは、2つの\s -> (i, s)をつなぐ際に最初
の\s -> (i, s)で出てきた値iをどうやって使いまわすか(束縛するか)も考えな
いといけないことです。

時間をおいてちょっと考えれば(考えないと天下り感が残りますが)、束縛とい
えばラムダ式なので二番目の\s -> (i, s)は最初のスタック操作の結果を受け
取って\s -> (i, s)を返すようにすればいいんじゃないかなーってことで

.. code-block:: haskell

    \i1 -> (\s -> (i2, s))

みたいな形にすれば良さげですね。

というわけでbindを定義してみます。上の関数は二番目の引数になっているの
がわかると思います。ちょっと関数の定義がくどくなってますが、括弧をつけ
て区切ったのでよく見ればわかると思います。

.. code-block:: haskell

    bind :: (Stack -> (Integer, Stack)) -> (Integer -> (Stack -> (Integer, Stack))) -> (Stack -> (Integer, Stack))
    bind opstack1 opstack2 = \s -> let (i1, s1) = opstack1 s
                                   in (opstack2 i1) s1

二番目の引数(opstack2)は最初のスタック操作の結果を受け取って束縛する関
数になっています。これの戻り値はナイスな感じにStack -> (Integer,
Stack)と最初の引数と同じ形の関数になっていますね。

ということはpushとpopの操作は何度でもbindすることができるということです。
また忘れていけないのはこれは関数であり初期状態を与えれば一気に計算が進
むということです。

お遊びとしてpopを三回する関数とpushを二回した後にpopを一回する関数にそ
れぞれlispっぽい名前を付けてみます(クダダダダー)。

.. code-block:: haskell

    poppp :: Stack -> (Integer, Stack)
    poppp = pop `bind` \_ -> pop `bind` \_ -> pop
    
    pushshop :: Stack -> (Integer, Stack)
    pushshop = (push 1) `bind` \_ -> (push 2) `bind` \_  -> pop

遊んでみます。

.. code-block:: sh

    *Main> let stack = [1..5]
    *Main> poppp stack
    (3,[4,5])
    *Main> pushshop  stack
    (2,[1,1,2,3,4,5])

これはこれで楽しいです。

加減乗除をbindを使って書きなおす
================================

bind関数はうまく動きそうなので、先に定義した **明示的に使いもしない状態
に名前をつけて** いて殘念臭を漂わせていたcalcを書き直します。

.. code-block:: haskell

    calc :: (Integer -> Integer -> Integer) -> Stack -> (Integer, Stack)
    calc op = pop `bind` \i1 -> pop `bind` \i2 -> push (op i1 i2)

    -- calc :: (Integer -> Integer -> Integer) -> Stack -> ((), Stack)
    -- calc op = \stack ->  
    --     let
    --         (i1, stack1) = pop stack
    --         (i2, stack2) = pop stack1
    --         (_, stack3)  = push (op i1 i2) stack2
    --     in
    --       ((), stack3)


もとのコードと比較して見るとわかりますが、新しく書きなおしたコードには
stackが明示的にでてこなくなりました。単にbindでpushとかpopをつないでい
るだけです。つまり、stackの状態はbind関数の中に隠されてしまったことにな
ります。

隠れたことによって、我々はスタックの状態を気にする必要がなくなりました。
これはスーパーマグナムウルトラハッピーなことですね。bind関数が素晴らし
すぎる。

もう一つ重要なことは(先にも書きましたが)、bindでつないだ関数は小さい関
数を順番につないで、もとの型と同じ形をしたより大きな1つの関数を返してい
ることです。そして初期状態を与えれば、その関数の内部で状態というものを
扱いつつ計算が一気に行われるわけです。

(私の疑問:そう見えるだけのか、計算の状態とはそもそもそういうものなのか?)

まぁ、これって状態を扱う計算の定石なんでデザインパターン的な名前がつい
ててもおかしくないんじゃないのかねー

実際についてますねー、モナドという名前が。

`モナドは値およびその値を使う計算の並びという観点からいえば、計算を構造化する方法です <http://www.sampou.org/haskell/a-a-monads/html/introduction.html>`_ 

Stateモナド
===========

構造の単位として状態をとって、状態をもとに計算した結果と、次の状態のペ
アを返す関数つまり \s -> (i1, s1)を中心に考えます。関数は長ったらしいの
で、適当な型を決めます。

.. code-block:: haskell

    newtype State s a = State { runState :: s -> (a, s) }

    instance Monad (State s) where
        return a = State $ \s -> (a, s)
        m >>= k  = State $ \s -> let
                (a, s') = runState m s
                in runState (k a) s'

>>=は先に実装したbindのことですね。最初に考えていたスタックはIntegerの
リストに別名を付けたものですが、OpはState [Integer] Integer型
に別名をつけたものです(そしてこれは実際には関数であることに注意)

.. code-block:: haskell

    import Control.Monad.State
    
    type Op = State [Integer] Integer
    
    push :: Integer -> Op
    push c = get >>= \cs -> put (c:cs) >>= \_ -> return c
    
    pop :: Op
    pop = get >>= \cs -> put (tail cs) >>= \_ -> return (head cs)
    
    calc :: (Integer -> Integer -> Integer) -> Op
    calc op = pop >>= \i1 -> pop >>= \i2 -> push (op i1 i2)
    
    add = calc (+)
    sub = calc (-)
    mul = calc (*)
    dvv = calc div
    
    poppp :: Op
    poppp = pop >>= \_ -> pop >>= \_ -> pop
    
    pushshop :: Op
    pushshop = (push 1) >>= \_ -> (push 2) >>= \_  -> pop

pushとpopはMonadStateクラスのメソッドであるputとgetを使って書きなおして
おきました。

これでだいぶすっきりしましたが、モナドはdo記法が使えるのでコレを使って
さらに書きなおしてみましょう

.. code-block:: haskell

    import Control.Monad.State
    
    type Op = State [Integer] Integer
    
    push :: Integer -> Op
    push c = do 
      cs <- get
      put (c:cs)
      return c
    
    pop :: Op
    pop = do
      cs <- get
      put (tail cs)
      return (head cs)
    
    calc :: (Integer -> Integer -> Integer) -> Op
    calc op = do
      i1 <- pop
      i2 <- pop
      push (op i1 i2)
    
    add = calc (+)
    sub = calc (-)
    mul = calc (*)
    dvv = calc div
    
    poppp :: Op
    poppp = do pop; pop; pop
    
    pushshop :: Op
    pushshop = do push 1; push 2; pop

一見手続き型の言語で書いてるようにみえますね。でも気をつけてください、
これは関数からより大きな関数をつくっているのであって、逐次実行をしてい
るわけではないのです。

(そもそも逐次実行とは何なのか?)

IOとは何なのか?
===============

さて、Stateモナドが分かっていると似たような状態付き計算が思い浮かぶと思
います。あれは結局どういう状態を初期値として与えてんだ?と疑問に思ったあ
なたは:i IOをする時が来たのです(ghciで)。

.. code-block:: haskell

    newtype IO a
      = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
                      -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
              -- Defined in `GHC.Types'

これは実世界という状態を取るStateモナドのことですね。特殊化されたState
モナドと考えていいんでしょう。(本当にいいのか?)

Writerモナド、ReaderモナドもStateモナドの特殊な状態
===================================================

Writerモナドは、ログが肥大化する状態を実装したStateモナドと考えればいいですね。

参考

- `State MonadとWriter Monadを見比べる <http://d.hatena.ne.jp/enakai00/20110726/1311651155>`_ 

Readerモナドは実行時にセットして後は読み出ししか出来ないStateモナドと考えればいいです。

..

    プログラムの実行時に決まるような大域的な情報を、プログラムの何個所かで共有するにはどうすればよいでしょうか？

- `第29回　グローバル変数の代わりに使えるReaderモナドとWriterモナド <http://itpro.nikkeibp.co.jp/article/COLUMN/20090303/325807/>`_ 

その他
======

MaybeとかEitherもStateモナドで書き直せるんだろうか?


まとめ
======

`モナドは (途中略) 計算を構造化する方法です <http://www.sampou.org/haskell/a-a-monads/html/introduction.html>`_ 

状態を純粋関数型言語で扱うための枠組みというかパターンがあって、それは
モナドという計算を構造化する型で表現できることがわかりました。これは
Stateモナドを理解して、その特殊な状態として他のモナドを理解すれば効率的
なんじゃないかなーと思います。





