def qpng01 = ({
        def argIsMethod(arg: Expression): Boolean = {
          cpg.method.nameExact(arg.code).where(_.codeNot("<empty>")).nonEmpty
        }

        def callsWithMethodAsParam = {
          cpg.call.nameNot("<operator>.*").where(_.argument.filter(arg => argIsMethod(arg)))
        }

        def argsDistinct(call: Call): Boolean = {
          def tmp = call.argument.isIdentifier.code
          return (tmp.l.distinct.size != tmp.l.size)
        }

        def isCorrectMethod(call: Call): Boolean = {
          if (call.argument.size != 3) return false
          def callee = call.callee
          def tmp = callee.call.nameNot("<operator>.*").where(_.reachableBy(callee.parameter.index(3)))
          def assignment = tmp.postDominatedBy.assignment.isCall.reachableBy(callee.parameter.index(1))
          assignment.nonEmpty
        }

        def isCallChainToFree(call: Call): Boolean = {
          def getMethod(arg: String) = cpg.method.nameExact(arg)
          def tmp = getMethod(call.argument(2).code).call.nameNot("<operator>.*")
            .callee.call.nameNot("<operator>.*")
            .callee.call.nameNot("<operator>.*")

          tmp.name("free").nonEmpty
        }

	//Query:
        callsWithMethodAsParam.filter(call => argsDistinct(call))
          .filter(call => isCorrectMethod(call)).filter(call => isCallChainToFree(call))
      })