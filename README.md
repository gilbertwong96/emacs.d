# Gilbert's Emacs Configuration

A modern, feature-rich Emacs configuration focused on productivity, development workflow, and an enhanced editing experience. This configuration uses [straight.el](https://github.com/radian-software/straight.el) for package management and is organized into modular components for easy maintenance.

![Screenshot](https://img.shields.io/badge/Emacs-29.1+-blue.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)

## ✨ Features

### 🎨 Theme & UI
- **Doom Themes** - Beautiful color schemes (default: doom-monokai-classic)
- **All-the-icons** - Icon set for various UI elements
- **Doom Modeline** - Fancy and informative modeline
- **Transparency support** - Adjustable frame transparency
- **Custom fonts** - JetBrains Mono with CJK support

### 🔧 Core Editor Features
- **Evil Mode** - Vim-like editing with Evil Collection
- **Which-key** - Interactive keybinding discovery
- **General** - Unified keybinding system with leader keys
- **Vertico** - Modern vertical completion UI
- **Corfu** - Auto-completion framework
- **Orderless** - Advanced completion filtering
- **Marginalia** - Annotations for completion candidates

### 🧠 AI Integration
- **GPTel** - AI chat integration with multiple providers
- **MCP (Model Context Protocol)** - Advanced AI tool integration
- **Claude Code IDE** - Direct Claude AI integration
- **Minuet** - AI-powered code completion
- **Aidermacs** - AI-assisted coding

- **Supported AI Providers:**
  - OpenAI (GPT-4.1-mini)
  - OpenRouter
  - Google Gemini
  - DeepSeek
  - Context7
  - Brave Search

### 📚 Knowledge Management
- **Org Mode** - Powerful note-taking and organization
- **Org-roam** - Zettelkasten-style note linking
- **Deft** - Quick note browsing and searching
- **Treemacs** - File tree sidebar with project management

### 💻 Development Tools

#### Language Support
- **LSP** - Language Server Protocol with Eglot
- **Tree-sitter** - Advanced syntax highlighting
- **Multi-language modes** - Go, Python, Rust, JavaScript, TypeScript, Web, Haskell, Erlang, Elixir, Elisp, Shell, Docker, Configuration files

#### Version Control
- **Magit** - Complete Git interface
- **Git-timemachine** - Browse file history
- **Git-gutter** - See changes in the gutter

#### Additional Tools
- **Docker** - Container management integration
- **Projectile** - Project management
- **YASnippet** - Template system
- **Flycheck** - On-the-fly syntax checking

### 🌟 Enhancements
- **EditorConfig** - Per-project configuration
- **Rainbow delimiters** - Color-coded brackets
- **Ligatures** - Programming symbol prettification
- **Iedit** - Multiple cursors editing
- **Page-break-lines** - Visual page breaks

## 🚀 Installation

### Prerequisites
- Emacs 29.1 or later
- Git
- [1Password CLI](https://developer.1password.com/docs/cli/) (for AI API keys)

### Quick Start

1. **Backup your existing configuration:**
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   ```

2. **Clone this repository:**
   ```bash
   git clone https://github.com/gilbertwong96/.emacs.d.git ~/.emacs.d
   ```

3. **Launch Emacs:**
   ```bash
   emacs
   ```

The configuration will automatically:
- Install straight.el package manager
- Download and configure all packages
- Set up the theme and UI

### 1Password Setup (Optional)

For AI integration, store your API keys in 1Password with these paths:
- `op://AI/OpenRouter-Gemini/credential`
- `op://AI/Google AI Studio API/credential`
- `op://AI/deepseek-api-key/password`
- `op://AI/OpenAI/credential`
- `op://AI/Context7/credential`
- `op://AI/BraveAPI/credential`

## 📁 Configuration Structure

```
~/.emacs.d/
├── init.el                    # Main configuration file
├── elisp/                     # Modular configuration files
│   ├── init-theme.el         # Theme and appearance
│   ├── init-settings.el      # Basic Emacs settings
│   ├── init-keybindings.el   # Keybinding configuration
│   ├── init-evil.el          # Vim emulation
│   ├── init-autocomplete.el  # Completion system
│   ├── init-vertico.el       # Vertical completion UI
│   ├── init-org.el           # Org mode configuration
│   ├── init-ai.el            # AI tools integration
│   ├── init-lsp.el           # Language Server Protocol
│   ├── init-treesit.el       # Tree-sitter syntax
│   ├── init-git.el           # Git integration
│   ├── init-terminal.el      # Terminal configuration
│   ├── init-[language].el    # Language-specific configs
│   └── ...
├── straight/                 # Package manager files
└── README.md                 # This file
```

## ⌨️ Keybindings

### Leader Keys
- **SPC** - Evil leader (Normal mode)
- **C-c** - Emacs leader
- **,** - Evil local leader
- **C-c l** - Emacs local leader

### Core Bindings
- **SPC f f** - Find file
- **SPC p f** - Find file in project
- **SPC b b** - Switch buffer
- **SPC w w** - Switch window
- **SPC TAB** - Toggle treemacs

### AI Integration
- **SPC C** - Claude Code IDE menu
- **SPC a** - Aidermacs menu
- **M-y** - AI completion (minibuffer)
- **M-i** - Show AI suggestion (overlay)

### Completion
- **M-p/n** - Navigate completion candidates
- **M-A** - Accept completion
- **M-a** - Accept completion line
- **M-e** - Dismiss suggestion

## 🎯 Language-Specific Features

### Go Development
- Go mode with LSP integration
- Automatic formatting on save
- Test execution support

### Python Development
- Python LSP server
- Virtual environment support
- Jupyter integration (optional)

### Web Development
- TypeScript, JavaScript, JSX/TSX support
- CSS/SCSS mode
- Web LSP servers

### Rust Development
- Rust Analyzer LSP
- Cargo integration
- Error checking with flycheck

## 🔧 Customization

### Adding New Packages

1. Find the appropriate `init-*.el` file for the feature
2. Add the package using `use-package`:
   ```elisp
   (use-package package-name
     :straight t
     :config
     ;; your configuration
     )
   ```

### Changing Theme

Edit `elisp/init-theme.el`:
```elisp
;; Change this line
(load-theme 'doom-monokai-classic t)
;; To something like
(load-theme 'doom-gruvbox-light t)
```

### Font Configuration

Edit `elisp/init-settings.el` in the `gilbert/set-font` function:
```elisp
(set-face-attribute 'default nil :family "Your Font Name")
(set-face-attribute 'default nil :height 130)
```

## 🐛 Troubleshooting

### Common Issues

1. **Packages not installing:**
   - Ensure you have a stable internet connection
   - Try removing the `straight/` directory and restart Emacs

2. **Font issues:**
   - Install JetBrains Mono Nerd Font
   - Check that the font is available to your system

3. **AI integration not working:**
   - Verify 1Password CLI is installed and authenticated
   - Check that API keys are stored in the correct vault paths

4. **Performance issues:**
   - Run `emacs --debug-init` to identify problematic configurations
   - Consider disabling unused modules in `init.el`

### Getting Help

- Use `SPC h b k` to describe a keybinding
- Use `SPC h v` to describe a variable
- Check the `*Messages*` buffer for error messages

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork this repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This configuration is released under the MIT License. See the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [straight.el](https://github.com/radian-software/straight.el) package manager
- Inspired by various configurations from the Emacs community
- Thanks to all package authors for their amazing work

## 📊 Statistics

- **31 configuration modules** covering different aspects of Emacs usage
- **15+ programming languages** supported with dedicated configurations
- **6 AI providers** integrated for enhanced productivity
- **Modular architecture** for easy customization and maintenance

---

**Happy Emacs! 🎉**
