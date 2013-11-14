package uk.gov.gds.model

object CodeLists {

  object BlpuStateCode extends Enumeration {
    type BlpuStateCode = Value
    val underConstruction, inUse, unoccupied, noLongerExists, planningPermissionGranted = Value

    def forId(id: Int) = id match {
      case 1 => Some(underConstruction)
      case 2 => Some(inUse)
      case 3 => Some(unoccupied)
      case 4 => Some(noLongerExists)
      case 6 => Some(planningPermissionGranted)
      case _ => None
    }
  }

  object LogicalStatusCode extends Enumeration {
    type LogicalStatusCode = Value
    val approved, alternative, provisional, historical = Value

    def forId(id: Int) = id match {
      case 1 => Some(approved)
      case 3 => Some(alternative)
      case 6 => Some(provisional)
      case 8 => Some(historical)
      case _ => None
    }
  }
}
