/* required-fields.js — mark required-but-still-empty inputs.
 *
 * Registers one custom message handler, `pma_required_fields`, whose payload is
 *   { all: [<input id>, ...], unset: [<input id>, ...], armed: <bool> }
 * `all` is every id the sender manages, `unset` the subset that is currently
 * blank; the CSS class `.pma-required-unset` (see www/shadcn.css) is added to
 * the matching .shiny-input-container and removed from the rest.
 *
 * TWO TIERS. `.pma-required-unset` alone is the muted "required" pill, shown
 * from the first paint so the reviewer can see what the form is asking for.
 * `armed` adds `.pma-required-armed` alongside it, and the CSS turns the pair
 * destructive-red; the server sets it only once the reviewer has actually
 * asked for an analysis, so a fresh form is never painted red.
 *
 * The server (step2_server) only sends a message when something it watches
 * changes, but app.R rebuilds the Step 2 body with renderUI on every step
 * change, which throws the DOM - and the class with it - away. The flags are
 * therefore cached on `window`, this file is loaded as part of the Step 2 body
 * so it re-executes on every rebuild, and the re-execution repaints from that
 * cache. Anything that re-runs here must stay idempotent for the same reason.
 */
(function () {
  'use strict';

  var MESSAGE = 'pma_required_fields';
  var CLASS = 'pma-required-unset';
  var ARMED_CLASS = 'pma-required-armed';

  // id -> is currently blank. Deliberately on window, not in this closure:
  // see the note above about the renderUI rebuild.
  var state = window.pmaRequiredUnset || (window.pmaRequiredUnset = {});

  // Cached beside `state`, and for the same reason: a rebuild has to be able
  // to repaint the armed tier without waiting for the next message.
  if (typeof window.pmaRequiredArmed === 'undefined') {
    window.pmaRequiredArmed = false;
  }

  function containerOf(id) {
    var el = document.getElementById(id);
    if (!el) return null;
    // radioButtons() / checkboxGroupInput(): the element carrying the input id
    // IS the container. textInput() and friends put the id on the <input>.
    if (el.classList && el.classList.contains('shiny-input-container')) return el;
    return el.closest ? el.closest('.shiny-input-container') : null;
  }

  // A mark nobody can see is not a mark. The Step 2 sidebar is a bslib
  // accordion, so a blank required field may sit inside a collapsed panel;
  // open it.
  //
  // Only when ARMED, and only once per panel per DOM build. Before the
  // reviewer has asked for an analysis the panel state is theirs to choose,
  // and the column selects are legitimately blank for the first few hundred
  // milliseconds of every build while the server populates them - opening on
  // that would fight the user and flash on every return from Step 3. The
  // once-only latch is stored on the panel element, which the renderUI
  // rebuild throws away along with the rest of the DOM, so the next Run
  // analysis on a fresh body opens it again.
  function reveal(box) {
    if (window.pmaRequiredArmed !== true) return;
    if (!box.closest) return;
    var panel = box.closest('.accordion-collapse');
    if (!panel || panel.classList.contains('show')) return;
    if (panel.dataset && panel.dataset.pmaRevealed === '1') return;
    if (panel.dataset) panel.dataset.pmaRevealed = '1';
    panel.classList.add('show');
    // Navigate to the toggle through the item rather than by id: bslib
    // generates the panel id at render time and it needs escaping in a
    // selector.
    var item = panel.closest('.accordion-item');
    var btn = item && item.querySelector('.accordion-button');
    if (btn) {
      btn.classList.remove('collapsed');
      btn.setAttribute('aria-expanded', 'true');
    }
  }

  function applyAll() {
    var armed = window.pmaRequiredArmed === true;
    Object.keys(state).forEach(function (id) {
      var box = containerOf(id);
      if (!box) return;
      if (state[id]) box.classList.add(CLASS);
      else box.classList.remove(CLASS);
      // The armed class is only ever meaningful together with CLASS, but it
      // is removed unconditionally so a disarm (a new outcome) cannot leave
      // a red border behind on a field that is still blank.
      if (state[id] && armed) box.classList.add(ARMED_CLASS);
      else box.classList.remove(ARMED_CLASS);
      if (state[id]) reveal(box);
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // Registering the same message type again replaces the previous handler
    // rather than erroring, so this is safe on every re-execution.
    Shiny.addCustomMessageHandler(MESSAGE, function (msg) {
      var all = (msg && msg.all) || [];
      var unset = (msg && msg.unset) || [];
      for (var i = 0; i < all.length; i++) {
        state[all[i]] = unset.indexOf(all[i]) !== -1;
      }
      if (msg && typeof msg.armed !== 'undefined') {
        window.pmaRequiredArmed = msg.armed === true;
      }
      applyAll();
    });
  }

  // Repaint the freshly built body from the cache (no message is in flight
  // when the user merely navigates back to Step 2 without changing anything).
  applyAll();

  // Belt and braces: outputs that render after this script runs (the Step 2
  // body arrives as one output, but the sidebar contains uiOutputs of its own)
  // get another pass. Bound once per page - the guard is what keeps repeated
  // executions of this file from stacking listeners.
  if (!window.pmaRequiredBound) {
    window.pmaRequiredBound = true;
    var jq = window.jQuery || window.$;
    if (jq) {
      jq(document).on('shiny:value', function () {
        setTimeout(applyAll, 0);
      });
    }
  }
})();
