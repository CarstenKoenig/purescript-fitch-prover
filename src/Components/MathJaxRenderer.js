"use strict";

exports.typeSetPage = function () {
    function cleanUp() {
        var existingNodes = document.querySelectorAll('.math');
        existingNodes.forEach(function (mathNode) {
            var mathJaxNodes = mathNode.querySelectorAll('.MathJax');
            if (mathJaxNodes.length > 0 && mathNode.innerText !== "") {
                // there is a formula text here - so MathJax will replace it
                // get rid of the already rendered children
                mathJaxNodes.forEach(function (n) { n.remove(); });
            }
        });
    }

    try {
        cleanUp();
        MathJax.typesetClear();
        MathJax.typeset();
    } catch (e) { }
}