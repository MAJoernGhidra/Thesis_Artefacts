def qphp01_refined = ({
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

        def extractOperation(in: Set[Call], filter: String): Set[Call] = {
          var erg: Set[Call] = Set()
          var workingSet: Set[Call] = in
          while (workingSet.nonEmpty) {
            for (call <- workingSet) {
              if (call.isCallTo(filter).nonEmpty){
                erg += call
              }
              else {
                workingSet ++= call.argument.isCall.toSet
              }
              workingSet -= call
            }
          }
          erg
        }

        def correctCondition(cond: Call): Boolean = {
          val filter1 = "<operator>.logicalOr|<operator>.logicalAnd"
          val filter2 = "<operator>.notEquals|<operator>.equals"
          var subConds = dismantle(cond, filter1)
          subConds = extractOperation(subConds, filter2) //changed

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

        def getCorrectLoop: Traversal[ControlStructure] = {
          def innerLoop = cpg.controlStructure.controlStructureType("FOR|DO|WHILE")
            .where(_.condition.isCall.filter(c => correctCondition(c)))

          val vulnerableInOuterLoop = innerLoop.condition.isCallTo("<operator>.logicalOr").nonEmpty

          if(vulnerableInOuterLoop) innerLoop.inAstMinusLeaf.isControlStructure.controlStructureType("FOR|DO|WHILE")
          else innerLoop
        }

        def incorrectCallInsideLoop(contSt: ControlStructure): Boolean = {
          def operation = contSt.ast.isCallTo("memchr").argument(3).ast
            .isLiteral.inCall
          val erg1 = operation.isCallTo("<operator>.addition").nonEmpty
          val erg2 = operation.isCallTo("<operator>.subtraction").argument(1).isLiteral.nonEmpty

          erg1 || erg2
        }

	//Query:
        getCorrectLoop.filter(l => incorrectCallInsideLoop(l))
      })