/* embed-height.js — 埋め込み親ページへコンテンツの高さを通知する。
 *
 * iframe で埋め込まれているときだけ、{ type: 'embed:height', height: <px> } を
 * 親へ postMessage する。送るのは高さの数値だけなので targetOrigin は '*'。
 * 受け取る側で origin と event.source を検証すること。
 * 直接開いているとき（window.parent === window）は何もしない。
 *
 * Shiny 版の追加事情: uiOutput でステップ本体を丸ごと差し替え、プロットは
 * 非同期に描画される。DOM の変化だけを見ていると描画完了前の高さを送るので、
 * Shiny のイベント（shiny:value / shiny:recalculated / shiny:visualchange /
 * shiny:idle）でも再計測する。
 */
(function () {
  'use strict';

  var embedded = window.parent !== window;

  // ステップ切り替え時に、親ページ側をツールの先頭までスクロールさせる。
  // 埋め込みでないときは何もしない no-op。
  window.pmaNotifyScrollTop = function () {
    if (!embedded) return;
    window.parent.postMessage({ type: 'embed:scrolltop' }, '*');
  };

  if (!embedded) return;

  var last = 0;

  // documentElement.scrollHeight は iframe のビューポート高より小さくならず、
  // body に min-height が付いていると縮まなくなる。body の子要素の実測を使う。
  function measure() {
    var body = document.body;
    if (!body) return 0;
    var kids = body.children;
    var bottom = 0;
    for (var i = 0; i < kids.length; i++) {
      // Shiny 1.13 の busy-indicator は body 直下に <svg> を挿す。SVGElement は
      // offsetTop / offsetHeight を持たず NaN になり bottom を汚すのでスキップする。
      var top = kids[i].offsetTop;
      var h = kids[i].offsetHeight;
      if (typeof top !== 'number' || typeof h !== 'number') continue;
      bottom = Math.max(bottom, top + h);
    }
    if (!bottom) return Math.ceil(document.documentElement.scrollHeight);
    var pad = parseFloat(getComputedStyle(body).paddingBottom) || 0;
    return Math.ceil(bottom + pad);
  }

  // 初回描画が済むまで通知を止めるゲート。開く前の DOM は Shiny 接続前の骨組み
  // だけで実高よりずっと小さく、送ると親の iframe が一度つぶれてから伸びる。
  // 開くのは「shiny:value を見たあとの shiny:idle」か、下の 5 秒タイマー（保険）。
  // idle 単独では駄目で、Shiny は出力を伴わない初回 flush でも idle を出す。実測
  // では t=968ms の 1 回目の idle 時点でまだ骨組み 210px だった（実高は 968px）。
  var ready = false;
  var sawValue = false;

  function notify() {
    if (!ready) return;
    var height = measure();
    if (!height || Math.abs(height - last) < 2) return;
    last = height;
    window.parent.postMessage({ type: 'embed:height', height: height }, '*');
  }

  // Shiny の再描画は 1 操作で何度もイベントを飛ばすのでまとめる。rAF は iframe が
  // オフスクリーン／タブが非アクティブだと走らず、その間の変化を取りこぼすので使わない。
  var pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    setTimeout(function () {
      pending = false;
      notify();
    }, 16);
  }

  // shiny:* は Shiny が jQuery の .trigger() で発火させる jQuery イベントで、
  // document.addEventListener には届かない（native リスナは一度も呼ばれない）。
  // native 版に書き換えると Shiny 由来の再計測が全部黙って死ぬので注意。
  // jQuery が無い環境向けに native も張るが、現行 Shiny では発火しないだけ。
  // 二重に届いても schedule() の debounce と 2px 閾値で潰れる。
  function onShiny(types, handler) {
    var jq = window.jQuery || window.$;
    if (jq) jq(document).on(types, handler);
    var list = types.split(' ');
    for (var i = 0; i < list.length; i++) {
      document.addEventListener(list[i], handler);
    }
  }

  function start() {
    if (typeof ResizeObserver === 'function') {
      var ro = new ResizeObserver(schedule);
      ro.observe(document.body);
      var kids = document.body.children;
      for (var i = 0; i < kids.length; i++) ro.observe(kids[i]);
      // 子要素の増減も拾う
      if (typeof MutationObserver === 'function') {
        new MutationObserver(function () {
          var k = document.body.children;
          for (var j = 0; j < k.length; j++) ro.observe(k[j]);
          schedule();
        }).observe(document.body, { childList: true, subtree: true });
      }
    } else if (typeof MutationObserver === 'function') {
      new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true, attributes: true });
    }
    window.addEventListener('resize', schedule);
    window.addEventListener('load', schedule);

    // Shiny の出力更新・再計算・プロット再描画のたびに測り直す。
    // shiny:value は「output が 1 つ以上描画された」印でもあるので控えておく。
    onShiny('shiny:value', function () {
      sawValue = true;
      schedule();
    });
    onShiny('shiny:recalculated shiny:visualchange', schedule);
    // 出力が描画されたあとの idle でゲートを開け、そこで初回の計測・通知を行う。
    // idle の直後はまだ画像が載りきっていないことがあるので、少し置いてもう一度。
    onShiny('shiny:idle', function () {
      if (!sawValue) return;
      ready = true;
      notify();
      setTimeout(notify, 150);
    });

    // 保険。上の shiny:idle を取り逃した場合（Shiny 側の実装変更でイベント名や
    // 発火方式が変わったとき）でも、高さが一切追従しない状態にはしない。
    setTimeout(function () {
      if (ready) return;
      ready = true;
      notify();
    }, 5000);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
