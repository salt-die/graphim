import std/macros
import fusion/matching

proc replaceArrayWithIterator(node: NimNode) =
  ## Modifies the formal parameters of a procDef NimNode in-place:
  ## `openArray` type is replaced with `iterator` type.
  for formalParamNode in node[3]:
    if formalParamNode.matches(
      IdentDefs[_, BracketExpr[@ident.eqIdent("openArray"), @identType], _]
    ):
      formalParamNode[1] = nnkIteratorTy.newTree(
        nnkFormalParams.newTree identType,
        newEmptyNode(),
      )

macro and_iterators_too*(body: untyped): untyped =
  ## Procs defined for openArray types will also be defined for iterators.
  for i in 0..<body.len:
    if body[i].kind == nnkProcDef:
      body.add body[i].copyNimTree
      body[^1].replaceArrayWithIterator

  result = body
