

# 简介

本文档是对配置文件的解释。


# 配置模块化原则

init-packages.el        # 插件管理
init-ui.el              # 视觉层配置
init-better-defaults.el # 增强内置功能
init-keybindings.el     # 快捷键绑定
init-org.el             # Org 模式相关的全部设定
custom.el              # 存放使用编辑器接口产生的配置信息


# Emacs 界面显示出现乱码


## 解决办法

M-x all-the-icons-install-fonts


# 字体设置

在 init-custom.el 中定义了两个字符串变量 english-font 和 chinese-font，分别表示 Emacs 中所使用的中英文字体名称。通过在 init-font.el 中调用函数 set-font 来修改 Emacs 的字体。set-font 函数定义如下：

    ;; init-font.el
    (defun set-font (english-font chinese-font)
      "Function for setting fonts.
    The `ENGLISH-FONT' and `CHINESE-FONT' are respectively
    the names of the English and Chinese font of Emacs."
      (interactive)
      ;;Setting English Font. Notice: the unit of `height' is 1/10 pt, so the 200 is 20 pt.
      (set-face-attribute
       'default nil :family english-font :height 200 :weight 'normal)
      ;;Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
    		      charset
    		      (font-spec :family chinese-font)))
      ;; tune rescale so that Chinese character width = 2 * English character width
      (setq face-font-rescale-alist '((english-font . 1.0) (chinese-font . 1.23)))
      )


# 主题设置

定义了一个字符串变量 my-theme，其表示 Emacs 当前的主题名称。在 init-ui.el 中修改 my-theme 变量为自己想加载的主题名称，然后通过 load-theme 函数加载新的主题。


# consult-ripgrep


## 依赖


### ripgrep - [GitHub - ripgrep](https://github.com/BurntSushi/ripgrep#installation)

在 macOS 上安装 ripgrep，代码如下：

    brew install ripgrep


# 背景虚化


## loop-alpha 函数


### 代码详解

-   lambda 返回一个匿名的函数，就将它假设为 fun(a,ab)，上边的语句就近似为“(fun (car h) (car (cdr h)))”，即 (car h) 和 (car (cdr h)) 为传入函数 fun 的两个参数。
-   let 定义了一个局部变量 h，值为(car alpha-list)，在 let 语句之外不没有定义。
-   ⚠️ 执行这个函数之后只是临时修改了 set-frame-paramete，重启后设置消失。如果想让设置永久生效，则需要直接在配置文件中修改。
-   ⚠️ 应该不能在 Emacs 启动后，通过函数来修改配置，即无法修改配置文件。利用函数只能暂时修改配置，重启之后配置重置。


### 代码

loop-alpha 函数来自 [EmacsWiki - alpha-window](https://www.emacswiki.org/emacs/alpha-window)，代码如下：

    (setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
    
    (defun loop-alpha ()
      (interactive)
      (let ((h (car alpha-list)))                ;; head value will set to
        ((lambda (a ab)
           (set-frame-parameter (selected-frame) 'alpha (list a ab))
           (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
           ) (car h) (car (cdr h)))
        (setq alpha-list (cdr (append alpha-list (list h))))
        )
      )


## 修改配置文件

利用 loop-alpha 函数无法永久修改配置，因此需要在配置文件中加入如下代码：

    (set-frame-parameter (selected-frame) 'alpha '(95 65))

