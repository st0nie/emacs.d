# emacs.d

我的emacs配置文件

## 安装方法

使用git
```sh
mv ~/.emacs.d ~/.emacs.d.old
git clone https://github.com/st0nie/emacs.d ~/.emacs.d/
```

## 特点

我使用god-mode，同时对emacs默认的键位做了一定修改。

- `C-z`作为折叠的前缀键位，god-mode中使用zt，zo，zc来控制折叠，类似vim的折叠控制。这个键位默认绑定是最小化，我认为是没什么用的
- `C-r`作为替换操作的快捷键，god-mode中，选择一段文字，按r即可删除文字并且关闭god-local-mode，类似于vim的c按键。如果没有选择文字，则会替换光标下的词，类似vim的ciw，如果没有词，则fallback到ctrl-d。
- `C-r`作为替换行操作的快捷键，god-mode中，选择一段文字，按R即可删除选取以及光标所在行并且关闭god-local-mode，类似vim的cc按键，如果没有选择文字，则删除光标所在行。
- `<escape>`作为激活god-mode的快捷键，类似vim由insert-mode切换到normal-mode。
- `i`用来取消激活god-mode，类似vim。
- minibuffer中按`<escape>`可以用来关闭minibuffer，类似vim的command-mode可以通过`<escape>`取消

## 不用evil的理由

用evil虽然模拟了绝大部分的操作，但某些行为是和vim不一样的，使用evil的话不少emacs命令需要按到ctrl，
如果用god-mode，可以省略ctrl的敲击，风格也和emacs本身更加接近。

vim的一些行为可以用god-mode来模拟，用emacs自带的功能与自己定义的命令来编辑文本，我认为可能是更好地选择
