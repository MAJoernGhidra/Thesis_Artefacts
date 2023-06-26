def qtif01 = ({
        def findCorrectLoop = {
          def dominatesIndirection(assign: operatorextension.OpNodes.Assignment): Boolean = {
            assign.dominates.assignment.target.isCallTo("<operator>.indirection")
              .ast.isIdentifier.codeExact(assign.target.ast.isIdentifier.code.l: _*).nonEmpty
          }
          def getDecrAssignment(assign: Traversal[operatorextension.OpNodes.Assignment]) = {
            assign.where(_.argument(2).isCallTo("<operator>.addition")
              .argument(2).code("-1"))
          }

          cpg.controlStructure.controlStructureType("DO|WHILE|FOR")
            .where(_.condition.isCallTo("<operator>.equals|<operator>.notEquals").argument.code("NULL|0"))
            .where(contSt => getDecrAssignment(contSt.ast.assignment).filter(ass => dominatesIndirection(ass)))
            .filter(contSt => contSt.condition.isCall.reachableBy(contSt.ast.assignment.target.ast.isIdentifier).nonEmpty)
        }

	//Query:
        findCorrectLoop
      })