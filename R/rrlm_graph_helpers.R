# ---- internal: Graphviz DOT string builder ---------------------------

#' Build a Graphviz DOT string from an rrlm_graph subgraph
#'
#' @param sub          igraph subgraph (already filtered to hubs + neighbours)
#' @param k            Number of hub nodes shown (used in title)
#' @param project_name Character(1) project name for the graph title
#' @param layout       Character(1) Graphviz layout engine
#' @return Character(1) DOT language string
#' @noRd
.rrlmgraph_to_dot <- function(sub, k, project_name, layout = "dot") {
  nt_sub <- igraph::vertex_attr(sub, "node_type")
  pr_sub <- as.numeric(igraph::vertex_attr(sub, "pagerank"))
  names_sub <- igraph::vertex_attr(sub, "name")

  if (is.null(nt_sub)) {
    nt_sub <- rep("function", igraph::vcount(sub))
  }
  if (is.null(names_sub)) {
    names_sub <- paste0("n", seq_len(igraph::vcount(sub)))
  }
  pr_sub[is.na(pr_sub)] <- 0

  # -- Type-to-colour maps -----------------------------------------------
  type_fill <- c(
    "function" = "#4682B4",
    "package" = "#C8D8E8",
    "testfile" = "#3CB371"
  )
  type_font <- c(
    "function" = "white",
    "package" = "#444444",
    "testfile" = "white"
  )

  # -- Node width scaled by PageRank (0.4 - 2.0 in); pkg/test fixed -----
  pr_range <- range(pr_sub)
  widths <- if (diff(pr_range) > 0) {
    0.4 + 1.6 * (pr_sub - pr_range[[1L]]) / diff(pr_range)
  } else {
    rep(0.8, length(pr_sub))
  }
  widths[nt_sub %in% c("package", "testfile")] <- 0.3

  # -- Bare labels (strip "file::" prefix) + DOT escaping ---------------
  bare_labels <- vapply(
    names_sub,
    function(nm) {
      lbl <- utils::tail(strsplit(nm, "::", fixed = TRUE)[[1L]], 1L)
      lbl <- gsub("\\", "\\\\", lbl, fixed = TRUE)
      gsub('"', '\\"', lbl, fixed = TRUE)
    },
    character(1L)
  )

  # -- Safe DOT node IDs (n1, n2, ...) ----------------------------------
  node_ids <- paste0("n", seq_along(names_sub))
  name_to_id <- stats::setNames(node_ids, names_sub)

  # -- Source-file prefix for cluster grouping (function nodes only) ----
  file_group <- vapply(
    names_sub,
    function(nm) {
      parts <- strsplit(nm, "::", fixed = TRUE)[[1L]]
      fg <- if (length(parts) >= 2L) parts[[1L]] else "other"
      gsub("[^A-Za-z0-9_]", "_", fg) # valid DOT subgraph id
    },
    character(1L)
  )

  # -- Node attribute lines ---------------------------------------------
  node_defs <- vapply(
    seq_along(names_sub),
    function(i) {
      nt <- nt_sub[[i]]
      fill <- type_fill[[nt]]
      if (is.na(fill) || is.null(fill)) {
        fill <- "#FFFACD"
      }
      fc <- type_font[[nt]]
      if (is.na(fc) || is.null(fc)) {
        fc <- "#222222"
      }
      sprintf(
        '  %s [label="%s", fillcolor="%s", fontcolor="%s", width=%.2f];',
        node_ids[[i]],
        bare_labels[[i]],
        fill,
        fc,
        round(widths[[i]], 2L)
      )
    },
    character(1L)
  )

  # -- Edge lines (edge_type is UPPERCASE in rrlmgraph) -----------------
  el <- igraph::as_data_frame(sub, what = "edges")
  edge_defs <- character(0L)
  if (nrow(el) > 0L) {
    et <- if ("edge_type" %in% names(el)) {
      toupper(el$edge_type)
    } else {
      rep("CALLS", nrow(el))
    }
    ec <- ifelse(
      et == "CALLS",
      "#888888",
      ifelse(
        et == "IMPORTS",
        "#4682B4",
        ifelse(et == "TESTS", "#3CB371", "#888888")
      )
    )
    fids <- ifelse(
      el$from %in% names(name_to_id),
      name_to_id[el$from],
      NA_character_
    )
    tids <- ifelse(
      el$to %in% names(name_to_id),
      name_to_id[el$to],
      NA_character_
    )
    ok <- !is.na(fids) & !is.na(tids)
    if (any(ok)) {
      edge_defs <- sprintf(
        '  %s -> %s [color="%s"];',
        fids[ok],
        tids[ok],
        ec[ok]
      )
    }
  }

  # -- Cluster subgraphs (function nodes only, grouped by source file) --
  fn_mask <- nt_sub == "function"
  clusters <- split(which(fn_mask), file_group[fn_mask])
  cluster_blocks <- vapply(
    names(clusters),
    function(fg) {
      idxs <- clusters[[fg]]
      inner <- paste(node_ids[idxs], collapse = "; ")
      fg_lbl <- gsub("_", ".", fg, fixed = TRUE) # restore . for display
      sprintf(
        '  subgraph cluster_%s {\n    label="%s.R";\n    style=dashed;\n    color="#AAAAAA";\n    fontcolor="#666666";\n    fontsize=10;\n    %s;\n  }',
        fg,
        fg_lbl,
        inner
      )
    },
    character(1L)
  )

  # -- Assemble ---------------------------------------------------------
  title <- paste0(project_name, "  \u2014  top ", k, " hubs")
  title <- gsub("\\", "\\\\", title, fixed = TRUE)
  title <- gsub('"', '\\"', title, fixed = TRUE)

  paste0(
    'digraph rrlmgraph {\n',
    sprintf(
      '  graph [layout=%s, rankdir=LR, fontname="Helvetica", label="%s", labelloc=t, fontsize=14, splines=ortho, nodesep=0.5, ranksep=0.8];\n',
      layout,
      title
    ),
    '  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=10, penwidth=0.5, margin=0.1];\n',
    '  edge  [arrowsize=0.5];\n\n',
    paste(cluster_blocks, collapse = "\n\n"),
    "\n\n",
    paste(node_defs, collapse = "\n"),
    "\n\n",
    paste(edge_defs, collapse = "\n"),
    "\n",
    '}\n'
  )
}
# ---- internal: pan/zoom JS injected via htmlwidgets::onRender ---------

#' Vanilla-JS pan/zoom handler for the grViz SVG widget
#'
#' Returns a JS function string for use with [htmlwidgets::onRender()].
#' Works by manipulating the SVG `viewBox` attribute -- no CDN, no external
#' library, no browser plugin.
#'
#' Interactions:
#'  * Mouse wheel -- zoom in/out centred on cursor position
#'  * Click-drag  -- pan
#'  * Pinch       -- zoom on touch devices
#'  * Double-click -- reset to the initial fit-to-screen view
#'
#' @return Character(1) JS function source accepted by [htmlwidgets::onRender()]
#' @noRd
.rrlmgraph_pan_zoom_js <- function() {
  "function(el) {
    // viz.js renders the SVG asynchronously; poll until it appears
    var MAX_WAIT = 3000, INTERVAL = 50, waited = 0;
    var timer = setInterval(function() {
      waited += INTERVAL;
      var svg = el.querySelector('svg');
      if (!svg && waited < MAX_WAIT) return;
      clearInterval(timer);
      if (!svg) return;

      // --- Read Graphviz natural dimensions ----------------------------
      var nw = parseFloat(svg.getAttribute('width'))  || svg.viewBox.baseVal.width  || 800;
      var nh = parseFloat(svg.getAttribute('height')) || svg.viewBox.baseVal.height || 600;

      // Remove fixed attrs so the SVG fills the container via CSS
      svg.removeAttribute('width');
      svg.removeAttribute('height');
      svg.style.display  = 'block';
      svg.style.width    = '100%';
      svg.style.height   = '100%';
      svg.style.cursor   = 'grab';
      svg.style.userSelect = 'none';

      // viewBox state: (ox, oy) = top-left corner; (vw, vh) = viewport size
      var ox = 0, oy = 0, vw = nw, vh = nh;
      function setVB() {
        svg.setAttribute('viewBox', ox + ' ' + oy + ' ' + vw + ' ' + vh);
      }
      setVB();

      // --- Mouse wheel zoom centred on cursor --------------------------
      el.addEventListener('wheel', function(e) {
        e.preventDefault();
        var rect  = svg.getBoundingClientRect();
        var px    = (e.clientX - rect.left)  / rect.width;   // 0-1 fraction
        var py    = (e.clientY - rect.top)   / rect.height;
        var factor = e.deltaY < 0 ? 0.85 : (1 / 0.85);
        var nvw = vw * factor, nvh = vh * factor;
        ox = ox + (vw - nvw) * px;
        oy = oy + (vh - nvh) * py;
        vw = nvw; vh = nvh;
        setVB();
      }, {passive: false});

      // --- Click-drag pan ----------------------------------------------
      var dragging = false, lastMX, lastMY;
      el.addEventListener('mousedown', function(e) {
        dragging = true;
        lastMX = e.clientX; lastMY = e.clientY;
        svg.style.cursor = 'grabbing';
        e.preventDefault();
      });
      window.addEventListener('mousemove', function(e) {
        if (!dragging) return;
        var rect = svg.getBoundingClientRect();
        ox -= (e.clientX - lastMX) / rect.width  * vw;
        oy -= (e.clientY - lastMY) / rect.height * vh;
        lastMX = e.clientX; lastMY = e.clientY;
        setVB();
      });
      window.addEventListener('mouseup', function() {
        if (dragging) { dragging = false; svg.style.cursor = 'grab'; }
      });

      // --- Touch: one-finger pan, two-finger pinch-zoom ----------------
      var lastTouches = null;
      el.addEventListener('touchstart', function(e) {
        e.preventDefault();
        lastTouches = Array.from(e.touches).map(function(t) {
          return {x: t.clientX, y: t.clientY};
        });
      }, {passive: false});
      el.addEventListener('touchmove', function(e) {
        e.preventDefault();
        var rect = svg.getBoundingClientRect();
        var cur  = Array.from(e.touches).map(function(t) {
          return {x: t.clientX, y: t.clientY};
        });
        if (cur.length === 1 && lastTouches && lastTouches.length >= 1) {
          ox -= (cur[0].x - lastTouches[0].x) / rect.width  * vw;
          oy -= (cur[0].y - lastTouches[0].y) / rect.height * vh;
        } else if (cur.length === 2 && lastTouches && lastTouches.length === 2) {
          var d0 = Math.hypot(lastTouches[1].x - lastTouches[0].x,
                              lastTouches[1].y - lastTouches[0].y);
          var d1 = Math.hypot(cur[1].x - cur[0].x, cur[1].y - cur[0].y);
          if (d0 > 0) {
            var factor = d0 / d1;
            var cx = ((cur[0].x + cur[1].x) / 2 - rect.left)  / rect.width;
            var cy = ((cur[0].y + cur[1].y) / 2 - rect.top)   / rect.height;
            var nvw = vw * factor, nvh = vh * factor;
            ox = ox + (vw - nvw) * cx;
            oy = oy + (vh - nvh) * cy;
            vw = nvw; vh = nvh;
          }
        }
        lastTouches = cur;
        setVB();
      }, {passive: false});

      // --- Double-click: reset to original fit-to-screen ---------------
      el.addEventListener('dblclick', function() {
        ox = 0; oy = 0; vw = nw; vh = nh;
        setVB();
      });
    }, INTERVAL);
  }"
}
