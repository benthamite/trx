# `trx`: Interface to a Transmission session

`trx` is an Emacs interface for the [Transmission](https://transmissionbt.com/) BitTorrent client. It communicates with a Transmission daemon over its JSON RPC protocol, giving you full torrent management without leaving Emacs.

The package is a renamed and improved fork of Mark Oteiza's `transmission.el`. It adds TLS support, torrent list filtering (by name, status, or label), per-torrent detail buffers, file renaming, labels, a session data cache, and more robust connection handling with timeouts and automatic retries.

The interface is organized around four views, each in its own major mode:

- **Torrent list** — the main entry point. Add, start, stop, remove, verify, and label torrents; set speed and ratio limits; filter by name, status, or label; navigate to per-torrent details.
- **File list** — toggle files for download, set per-file priorities, visit or copy files on disk, rename files, or open them with external applications.
- **Torrent info** — detailed metadata: dates, tracker statistics, peer origins, piece visualization, speed limits, and more.
- **Peer list** — connected peers with addresses, flags, progress, transfer rates, client names, and optional geolocation.

All views support marking items for batch operations and optional automatic refresh via a configurable timer. A global minor mode (`trx-turtle-mode`) toggles Transmission's alternative speed limits and shows the active limits on the mode line.

## Screenshots

![trx in action](example.png)

## Installation

`trx` requires Emacs 24.4 or later.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(use-package trx
  :vc (:url "https://github.com/benthamite/trx"))
```

### Elpaca

```emacs-lisp
(use-package trx
  :ensure (:host github :repo "benthamite/trx"))
```

### straight.el

```emacs-lisp
(use-package trx
  :straight (:host github :repo "benthamite/trx"))
```

## Quick start

```emacs-lisp
;; Optional: enable auto-refresh in the torrent list
(setopt trx-refresh-modes '(trx-mode))

;; Optional: configure RPC credentials
(setopt trx-rpc-auth '(:username "your-username"))
;; The password is looked up via auth-source if not set explicitly.
```

Then run `M-x trx` to open the torrent list, or `M-x trx-add` to add a torrent by URL, magnet link, info hash, or file path.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](https://stafforini.com/notes/trx/).

## License

`trx` is licensed under the [GNU General Public License v3.0](LICENSE).
