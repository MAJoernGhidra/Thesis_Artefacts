def qssl02 = ({
        def getOrOperations = {
          cpg.call("<operator>.or").filter(call => call.argument(1)
            .isCallTo("<operator>.shiftLeft").argument(2).isLiteral.codeExact("8").nonEmpty)
            .filter(call => call.argument(2)
              .isCallTo("<operator>.arithmeticShiftRight|<operator>.indirectIndexAccess").argument(2)
              .isLiteral.code("8|1").nonEmpty)
        }

        def getControlingConditions(call: Call) = {
          val filter = "<operator>.logicalOr|<operator>.logicalAnd"
          var args: Set[Call] = Set()
          var orAndConds: Set[Call] = Set()
          call.argument.isCall.foreach(arg =>
            if (arg.isCallTo(filter).nonEmpty) orAndConds += arg
            else args += arg
          )

          while (orAndConds.nonEmpty) {
            var tmp: Set[Call] = Set()
            orAndConds.foreach(c => {
              c.argument.isCall.foreach(arg =>
                if (arg.isCallTo(filter).nonEmpty) orAndConds += arg
                else args += arg)
              tmp += c
            }
            )
            orAndConds --= tmp
          }

          def checkForAmountOfEqualOps(in: Set[Call]) = {
            val filter = "<operator>.lessThan|<operator>.notEquals|<operator>.equals"
            val filterEq = "<operator>.notEquals|<operator>.equals"
            var conds: Set[Call] = in
            var erg: Set[Call] = Set()
            erg = conds.filter(con => con
              .isCallTo(filter).nonEmpty)
            conds --= erg

            while (conds.nonEmpty) {
              var tmp: Set[Call] = Set()
              conds.foreach(c => {
                c.argument.isCall.foreach(arg =>
                  if (arg.isCallTo(filter).nonEmpty) erg += arg else conds += arg
                )
                tmp += c
              }
              )
              conds --= tmp
            }
            var count = 0
            erg.foreach(c => if (c.isCallTo(filterEq).argument(2).code("NULL|0").nonEmpty) count += 1)
            if (erg.size == 3 && count == 3) true
            else false
          }

          checkForAmountOfEqualOps(args)
        }

        //Query:
        getOrOperations.controlledBy.isCall.filter(call => getControlingConditions(call)).method
      })