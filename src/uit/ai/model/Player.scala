package uit.ai.model

trait Player {
  /**
   * Hàm nhận vào bàn cờ và trả về nước đi tiếp theo
   * @param board trạng thái bàn cờ hiện tại. 
   * @param playerSide AI đang cầm quân nào? Cell có ba thể hiện là: Round, Circle và Blank
   * @return Tọa độ x, y của nước đi tiếp theo
   */
  def nextMove(board: CaroBoard, playerSide: Cell): (Int, Int)
  
  /**
   * Hàm trả về tên của người chơi
   * @return Tên người chơi
   */
  def getName: String
}