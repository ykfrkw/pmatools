/* flowchart.js — light up the branch a domain judgment actually took.
 *
 * Every `.pma-flowchart` wrapper carries an inlined SVG (see pma_flowchart()
 * in R/ui_helpers.R) plus `data-pma-path`, a space-separated list of element
 * ids inside that SVG. This file adds the class `pma-fc-on` to each of them;
 * www/shadcn.css does the rest, and does it with stroke WIDTH as well as
 * colour so the highlight survives a black-and-white print.
 *
 * The ids come from the package: each assessor records a `flow_path` fact
 * naming the nodes it traversed, and tests/testthat/test-flowchart-nodes.R
 * asserts those names exist in the figure. Nothing here rewrites the SVG —
 * the server ships the same bytes for every analysis and only the attribute
 * changes, so a wrong path is a data bug in one place rather than a
 * string-surgery bug in another.
 *
 * SAME SHAPE AS required-fields.js, and for the same reason: Step 3's body is
 * rebuilt by renderUI, which throws the DOM away, so this file is loaded as
 * part of that body and re-executes on every rebuild. Everything here must
 * therefore be idempotent, and the `shiny:value` listener is bound once,
 * behind a flag on `window`.
 */
(function () {
  'use strict';

  var ON = 'pma-fc-on';

  function paintOne(wrap) {
    var spec = wrap.getAttribute('data-pma-path') || '';
    var want = spec.split(/\s+/).filter(function (s) { return s.length > 0; });

    // Clear first: the wrapper may be a re-render of a previous analysis
    // whose path went elsewhere, and a stale highlight is worse than none.
    var lit = wrap.querySelectorAll('.' + ON);
    for (var i = 0; i < lit.length; i++) lit[i].classList.remove(ON);

    for (var j = 0; j < want.length; j++) {
      // querySelector, not getElementById: ids inside an inlined SVG are
      // document-global, and scoping the lookup to this wrapper keeps two
      // charts on one page from stealing each other's nodes.
      var el = wrap.querySelector('[id="' + want[j] + '"]');
      if (el && el.classList) el.classList.add(ON);
    }
  }

  function paintAll() {
    var wraps = document.querySelectorAll('.pma-flowchart[data-pma-path]');
    for (var i = 0; i < wraps.length; i++) paintOne(wraps[i]);
  }

  paintAll();

  // The charts arrive inside uiOutputs that render after this script runs, so
  // one pass at load time is not enough. Bound once per page; the guard is
  // what keeps repeated executions of this file from stacking listeners.
  if (!window.pmaFlowchartBound) {
    window.pmaFlowchartBound = true;
    var jq = window.jQuery || window.$;
    if (jq) {
      jq(document).on('shiny:value', function () {
        setTimeout(paintAll, 0);
      });
    }
  }
})();
