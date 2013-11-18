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

  object StreetRecordTypeCode extends Enumeration {
    type StreetRecordTypeCode = Value
    val officiallyDesignated, streetDescription, numberedStreet, unofficialStreetDescription, descriptionForLLPG = Value

    def forId(id: Int) = id match {
      case 1 => Some(officiallyDesignated)
      case 2 => Some(streetDescription)
      case 3 => Some(numberedStreet)
      case 4 => Some(unofficialStreetDescription)
      case 9 => Some(descriptionForLLPG)
      case _ => None
    }
  }

  object StreetStateCode extends Enumeration {
    type StreetStateCode = Value
    val streetUnderConstruction, open, closed = Value

    def forId(id: Int) = id match {
      case 1 => Some(streetUnderConstruction)
      case 2 => Some(open)
      case 4 => Some(closed)
      case _ => None
    }
  }

  object StreetSurfaceCode extends Enumeration {
    type StreetSurfaceCode = Value
    val metalled, unMetalled, mixed = Value

    def forId(id: Int) = id match {
      case 1 => Some(metalled)
      case 2 => Some(unMetalled)
      case 3 => Some(mixed)
      case _ => None
    }
  }

  object StreetClassificationCode extends Enumeration {
    type StreetClassificationCode = Value
    val footpath, cycleway, allVehicles, restricted, bridleway = Value

    def forId(id: Int) = id match {
      case 4 => Some(footpath)
      case 6 => Some(cycleway)
      case 8 => Some(allVehicles)
      case 9 => Some(restricted)
      case 10 => Some(bridleway)
      case _ => None
    }
  }
}
