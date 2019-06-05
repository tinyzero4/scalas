package observatory

object Common {
  private[observatory] val RADIUS = 6371.0
  private[observatory] val POWER = 6
  private[observatory] val LAT_MAX = 90
  private[observatory] val LON_MAX = 180
  private[observatory] val ALPHA_LEVEL = 127
  private[observatory] val ZERO = 0.0
  private[observatory] val SUB_TILE_ZOOM = 8
  private[observatory] val TILE_SIZE = Math.pow(2, SUB_TILE_ZOOM).toInt
  private[observatory] val GRID_LAT_MIN = -89
  private[observatory] val GRID_LAT_MAX = 90
  private[observatory] val GRID_LON_MIN = -180
  private[observatory] val GRID_LON_MAX = 179

}
