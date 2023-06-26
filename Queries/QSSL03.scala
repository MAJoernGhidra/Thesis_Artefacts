def qssl03 = ({
        def getAllAssignments = {
          cpg.assignment.where(_.argument(2).isCall.filter(call => call.argument.size == 4)
            .argument(2).code("NULL|0"))
        }
        def flowToOr(assign: operatorextension.OpNodes.Assignment) = {
          def met = assign.method
          def sink = met.call("<operator>.or")
          sink.reachableByFlows(assign.target).nonEmpty

        }

	//Query:
        getAllAssignments.filter(assign => flowToOr(assign))
      })