def qphp01 = ({
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

        def correctCondition(cond: Call): Boolean = {
          val filter1 = "<operator>.logicalOr|<operator>.logicalAnd"
          val filter2 = "<operator>.notEquals|<operator>.equals"
          val subConds = dismantle(cond, filter1)

          if (subConds.size != 3) return false
          var slash = false
          var lEnd = false
          for (c <- subConds) {
            if (c.isCallTo(filter2).nonEmpty) {
              if (c.argument(2).code == "'\\0'") lEnd = true
              if (c.argument(2).code == "'/'") slash = true
            }
          }
          slash && lEnd
        }

        def getCorrectLoop = {
          cpg.controlStructure.controlStructureType("FOR|DO|WHILE")
            .where(_.condition.isCall
              .filter(c => correctCondition(c)))
          .inAstMinusLeaf.isControlStructure.isDo
        }

        def incorrectCallInsideLoop(contSt: ControlStructure): Boolean = {
          contSt.ast.isCallTo("memchr").argument(3).ast.isLiteral.inCall.isCallTo("<operator>.addition").nonEmpty
        }

	//Query:
        getCorrectLoop.filter(l => incorrectCallInsideLoop(l))
      })