export function typeSetPage () {
  function prepare () {
    var existingNodes = document.querySelectorAll('.math')
    existingNodes.forEach(function (mathNode) {
      var mathJaxNodes = Array.from(mathNode.children).filter(e =>
        e.matches('.MathJax')
      )
      var allNodeCount = mathNode.children.length
      if (mathJaxNodes.length < allNodeCount) {
        // there is a formula text besides mathJax-content here
        // so MathJax will replace the text creating yet another mathjax-text
        // better get rid of the MathJax-nodes
        mathJaxNodes.forEach(function (n) {
          n.remove()
        })
      }
    })
  }

  function cleanUp () {
    var existingNodes = document.querySelectorAll('.math')
    existingNodes.forEach(function (mathNode) {
      var mathJaxNodes = Array.from(mathNode.children).filter(e =>
        e.matches('.MathJax')
      )
      // remove duplicates - every .math node should only have one .MathJax child-node
      if (mathJaxNodes.length > 1) {
        var removed = mathJaxNodes.splice(1, mathJaxNodes.length - 1)
        removed.forEach(n => n.remove())
      }
    })
  }

  try {
    prepare()
    MathJax.typesetClear()
    MathJax.typeset()
    cleanUp()
  } catch (e) {}
}
