

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


# consult-ripgrep


## 依赖


### ripgrep - [GitHub - ripgrep](https://github.com/BurntSushi/ripgrep#installation)

在 macOS 上安装 ripgrep，代码如下：

    brew install ripgrep

