qssl01_refined = ({
        def getIfStatements = {
          cpg.controlStructure.isIf
            .where(_.condition.isCallTo("<operator>.lessThan").argument(1).isCallTo("<operator>.addition"))
            .where(_.ast.isCallTo("memcpy"))
        }

        def getFlowToMemcpy(contSt: ControlStructure): Boolean = {
          def arg = contSt.condition.isCall.argument(1).isCall.argument
          def memcpy = contSt.ast.isCallTo("memcpy")
          def paramFlowToCond(args: Traversal[Expression], contSt: ControlStructure) = {
            def source = contSt.method.parameter
            args.reachableBy(source).nonEmpty
          }
          def flowFromAssignmentBeforeCondToMemcpy: Boolean = {
            def sink = memcpy.argument(1)
            def source = contSt.condition.postDominates.assignment
        
            sink.reachableBy(source.target.isIdentifier).nonEmpty
          }


          if (!paramFlowToCond(arg, contSt)) return false
          var erg1 = false
          var erg2 = false
          def flow1 = memcpy.argument(1).reachableByFlows(arg)
          def flow2 = memcpy.argument(3).reachableByFlows(arg)

          if (flow1.isEmpty) {
            erg1 = flowFromAssignmentBeforeCondToMemcpy
          }
          else if (flow1.head.elements.size < 4) erg1 = true

          if (flow2.nonEmpty && flow2.head.elements.size < 4) erg2 = true

          erg1 && erg2

        }

	//Query:
        getIfStatements.filter(contSt => getFlowToMemcpy(contSt))
      })