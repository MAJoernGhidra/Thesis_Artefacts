def qpng02 = ({
        def loops = {
          cpg.controlStructure.controlStructureType("DO|FOR|WHILE")
            .filter(contS => (contS.condition.isCall.reachableByFlows(contS.method.parameter).nonEmpty))
            .filter(contSt => contSt.ast.isControlStructure.size == 1)
        }

        def assignToSameId(contSt: ControlStructure): Boolean = {
          val ids = scala.collection.mutable.Map[String, Int]()
          def addToMap(inp: String) = {
            if (ids.contains(inp)) {
              ids.update(inp, ids(inp) + 1)
            }
            else ids += (inp -> 1)
          }
          contSt.ast.assignment.target.dedup.foreach(target => target.ast.isIdentifier.foreach(id => addToMap(id.code)))
          var forLoop = false
          var doOrWhileLoop = false
          for (value <- ids.values) {
            if (value == 4) doOrWhileLoop = true
            if (value == 3) forLoop = true
          }
          if (contSt.isWhile.nonEmpty || contSt.isDo.nonEmpty) return doOrWhileLoop
          if (contSt.isFor.nonEmpty) return forLoop
          false
        }

        def hasCorrectCondition(contSt: ControlStructure): Boolean = {
          contSt.condition.isCallTo("<operator>.lessThan").ast.isCallTo("<operator>.division").nonEmpty
        }

	//Query:
        loops.filter(l => assignToSameId(l)).filter(l => hasCorrectCondition(l))
      })