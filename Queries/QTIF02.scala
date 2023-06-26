def qtif02 = ({
        def dismantle(call: Call, filter: String): Set[Call] = {
          var workingSet: Set[Call] = Set(call)
          var erg: Set[Call] = Set()

          while (workingSet.nonEmpty) {
            var tmp = Set[Call]()
            workingSet.foreach(w => {
              w.argument.isCall.foreach(c => {
                if (c.isCallTo(filter).nonEmpty) workingSet += c else erg += c
              })
              tmp += w
            })
            workingSet --= tmp
          }
          erg
        }

        def getOuterLoop = {
          cpg.controlStructure.isDo.filter(contSt => contSt.condition
            .reachableBy(contSt.method.call("<operator>.shiftLeft").inAssignment.target.ast.isIdentifier).nonEmpty)
            .filter(_.astMinusRoot.isControlStructure.isDo.nonEmpty)
        }

        def correctDoLoop(contSt: ControlStructure):Boolean = {
          def getCorrectCondition(contSt: ControlStructure): Boolean = {
            val conds = dismantle(contSt.condition.isCall.l.head, "<operator>.logicalAnd")
            if (conds.size != 2) return false
            var lessThanEquals = false
            var notEquals = false
            for (c <- conds) {
              if (c.isCallTo("<operator>.lessEqualsThan").nonEmpty) lessThanEquals = true
              if (c.isCallTo("<operator>.notEquals").nonEmpty) notEquals = true
            }
            lessThanEquals && notEquals
          }

          contSt.astMinusRoot.isControlStructure.isDo.where(_.condition.isCallTo("<operator>.logicalAnd"))
            .filter(contSt => getCorrectCondition(contSt)).exists(_.ast.isCallTo(".*fprintf.*").nonEmpty)
        }

	//Query:
        getOuterLoop.filter(l => correctDoLoop(l))
      })