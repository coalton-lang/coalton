# mine: A Coalton and Common Lisp IDE

`mine` is a full-featured, TUI-based IDE for Coalton and Common Lisp. Inspired by QBASIC and Borland's Turbo products, it provides an all-in-one compiler/editor/debugger experience that is suitable for beginners and experts alike.

> [!CAUTION]
> `mine` is alpha-quality software. It has not been extensively tested by real users doing real work. Your data may get lost or corrupted. Please make sure to save often and make backups. `mine` has only been tested on relatively modern terminals with true color support, Unicode fonts, etc. Make sure your terminal is configured to accept `Alt` as a modifier key.

## Features

- "Just Works" application based on a single core binary that just depends on `libc` & co.
- Cross-platform on Windows, Mac, and Linux
- Common Lisp (via SBCL) and Coalton built-in
- Syntax highlighting and tab completion
- Support for interactive development with a REPL
- Structural editing and canonical indentation
- Native support for projects via ASDF
- Native interactive Lisp debugger
- Jump-to-definition (also known as "`M-.`")
- Function type and parameter list hints
- Build-executable feature
- Driven by either a mouse or keyboard or both with standard keybindings like Ctrl+c and Ctrl+v
- Written entirely in Coalton and Lisp!

There are also anti-features:

- No ads, no telemetry
- No plugins, themes, extensions
- No support for other languages
- No Git integration, no AI
- Not Emacs

## Downloading, Installing, and Running mine

`mine` can be downloaded from the [GitHub releases page](https://github.com/coalton-lang/coalton/releases).

**Windows**: Download the `mine-app` installer and run it. This will install the `mine` application onto your computer.

**Mac**: Download the `mine-app` dmg file, open it, and install `mine` into your Applications folder.

**Linux**: Download `mine-core`, unzip it, and put `mine` somewhere your PATH can see.

> [!NOTE]
> For Windows and Mac, the `mine-app` comes with everything you need; there are zero dependencies. We recommend this in order to have the smoothest experience possible. If you wish to use your own terminal, or you want to run `mine` over remote SSH connections, you can instead optionally download the `mine-core` variant, which is a single, statically linked executable.

Once installed, open `mine` and proceed to **F6:Site > Setup Wizard** to get setup!

> [!IMPORTANT]
> We really recommend running F6:Site > Setup Wizard (or `mine --setup`) before proceeding!

Quit `mine` with **F1:mine > Quit** menu or press **Ctrl+q**.

## Layout of `mine`

`mine` has a fixed layout:

- The **Open Files** pane: lists the current open files. It automatically collapses when editing. Switch to it with Ctrl+t. You can switch to (Enter), close (c), and save (s) files here.
- The **Project Tree**: lists all of the files in your `.asd` project. You can open files from this tree.
- The **Editor** pane: where you do all your editing. Switch to it with Ctrl+e.
- The **REPL** pane: where you interact with your program. Switch to it with Ctrl+r.
- The **Status Line**: shows recent actions, as well as context-sensitive information about your program.

Everything is clickable with your mouse, or accessible with keyboard shortcuts.

## Workflows

> [!TIP]
> Since `mine` is a relatively simple program, there aren't a lot of things to know about it operating it. Look at F1:mine > Help for a list of all of the keybindings.

The biggest difference between `mine` and conventional IDEs is an idea we call "beaming". Your program, whether your whole project or just an individual function, can be beamed to the REPL. That means making that component available to the REPL for immediate testing and use. This gives an entirely new *interactive* and *incremental* development workflow that looks something like:

1. Open files of interest. Or make a new one. Write some code.
2. Open a REPL with F3:Editor > Show REPL, or Ctrl+r. Flip between the REPL and editor with Ctrl+r and Ctrl+e.
3. Beam the code you want:
    - Your project with F5:Project > Beam Project, or `(asdf:load-system "your-project")`
    - A whole file with F3:Editor > Beam File, or Alt+k in the editor.
    - An individual toplevel form under the cursor with F3:Editor > Beam Toplevel Form, or Alt+c.
4. Go to the REPL and try it out. Edit code. Beam. Iterate.
5. Define a `main` function, and build an executable with F5:Project > Build Executable.


### Projects

A "project" is just a group of related files as defined by a `.asd` file. `mine` knows about projects, and can show project trees.

Create a new project with F5:Project > New Project. It will fill out a very basic project template. When you make new files, you can always add them to the project by going to F5:Project > Edit Project, adding the `:file` (for Lisp) or `:ct-file` (for Coalton) entries, adding new dependencies in `:depends-on`, and saving.

## Configuration

You can setup `mine` with `mine --setup` and not worry about manual configuration. But if you insist, an optional configuration file named `.mine` can go in your user home directory. It is an S-expression config file:

```lisp
(
  :<option> <value>
  :<option> <value>
  ...
)
```
Whitespace is arbitrary; add as many spaces and newlines as you please. The possible options are:

- `:structural-editing` [`t`, `nil`]: Enable or disable structural editing by default. Values are `t` (enable, default) or `nil` (disable).
- `:auto-hide-tree-pane` [`t`, `nil`]: Auto-hide the tree pane when it loses focus. Values are `t` (auto-hide, default) or `nil` (freeze).
- `:repl-language` [`"coalton"`, `"lisp"`]: The default language to boot the REPL as. The REPL language determines how forms are sent and interpreted by teh REPL.
- `:default-package` `<string>`: The default package to boot the REPL in and beam code from files in. Values are any boot-time package name, usually `"COALTON-USER"` (default) or `"CL-USER"`.
- `:repl-init` `<lisp-form>`: An arbitrary Lisp form to run in the REPL each time it starts up.
    - Examples: `(defvar *repl-started* t)` 
- `:project-roots` [`<string>`, `(<string> ...)`]: A string or a list of strings which are root directories where projects are subdirectories. This is where `mine` will search for projects.
    - Examples: `"c:/projects/"`, `("/projects/" "/my/other-projects/")`
- `:repl-heap-size-megabytes` `<integer>`: The number of megabytes of memory the REPL should get. Defaults to 4096 MiB.
- `:app-font-family` `<string>`: The font family to use in the mine desktop app. Defaults to `"Iosevka Fixed Slab Extended"` which comes pre-installed with the app.
- `:app-font-size` `<integer>`: The font size in pixels to use in the mine desktop app. Defaults to `14`.

## License

MIT
