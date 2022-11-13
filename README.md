

# 简介

本文档是对自己的 Emacs 配置做一些解释，方便以后查看。


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

