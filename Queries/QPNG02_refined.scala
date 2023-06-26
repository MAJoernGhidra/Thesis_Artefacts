def qpng02_refined = ({
        def loops = {
          cpg.controlStructure.controlStructureType("DO|FOR|WHILE")
            .filter(contSt => contSt.ast.isControlStructure.size == 1)
            .where(_.astMinusRoot.isCall.filter(_.argument.size == 3)) //added to find 'png_crc_read(param_1,&uStack_13,3);'
            .filter(contS => (contS.condition.isCall.reachableByFlows(contS.method.parameter).nonEmpty))
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
            if (value == 4) doOrWhileLoop = true // 3 assignments plus pointer increment
            if (value == 3) forLoop = true
          }
          if (contSt.isWhile.nonEmpty || contSt.isDo.nonEmpty) return doOrWhileLoop
          if (contSt.isFor.nonEmpty) return forLoop
          false
        }

        def hasCorrectCondition(contSt: ControlStructure): Boolean = {
          val erg1 = contSt.condition.isCallTo("<operator>.lessThan|<operator>.notEquals").ast.isCallTo("<operator>.division").nonEmpty
          if (!erg1) {
            return contSt.condition.ast.isIdentifier.dominatedBy.isCall
              .inAssignment.where(_.argument(2).isCallTo("<operator>.division")
              .argument.isLiteral.code("3")).nonEmpty
          }
          erg1
        }

	//Query:
        loops.filter(l => assignToSameId(l)).filter(l => hasCorrectCondition(l))
      })