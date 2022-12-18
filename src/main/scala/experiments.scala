import day16.{Valve, eventualPressureRelease}

object experiments {
  def chooseNextValve(valves: Set[Valve], currentValve: Valve, minutesRemaining: Int): Valve = {
    val candidates = valves - currentValve
    candidates.maxBy(v => eventualPressureRelease(valves, currentValve, v, minutesRemaining))
  }

  def rankCandidates(valves: Set[Valve], currentValve: Valve, minutesRemaining: Int): Array[Valve] = {
    valves.filter(v => v.flowRate > 0)
      .toArray
      .sortBy(candidate => eventualPressureRelease(valves, currentValve, candidate, minutesRemaining))
  }
}
