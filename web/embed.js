// Include on a page that embeds FuncProg Lab demos in iframes.
// Each demo reports its height; this sets the matching iframe's height.
(function () {
  if (window.__funcprogEmbed) return;
  window.__funcprogEmbed = true;
  window.addEventListener('message', function (e) {
    var d = e.data;
    if (!d || d.type !== 'funcprog:resize' || typeof d.height !== 'number') return;
    var frames = document.getElementsByTagName('iframe');
    for (var i = 0; i < frames.length; i++) {
      if (frames[i].contentWindow === e.source) frames[i].style.height = d.height + 'px';
    }
  });
})();
