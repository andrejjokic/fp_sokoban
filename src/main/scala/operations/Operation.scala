package sokoban
package operations

import model.Matrix

type Function = Matrix => Matrix

abstract class Operation extends Function {

}
