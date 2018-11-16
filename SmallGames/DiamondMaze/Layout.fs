namespace Grumpy.DiamondMaze

module Layout =
    open Grumpy.Common

    let cellColumns = 40
    let cellRows = 30
    let cellWidth = 16
    let cellHeight = 16

    //field frame (0,0,29,29)
    let fieldFrameOffsetX = 0
    let fieldFrameOffsetY = 0
    let fieldFrameColumns = 29
    let fieldFrameRows = 29

    //panel frame (29,0,1,29)
    let panelFrameOffsetX = 29
    let panelFrameOffsetY = 0
    let panelFrameColumns = 11
    let panelFrameRows = 29

    //field (1,1,27,27)
    let fieldOffsetColumn = 1
    let fieldOffsetRow = 1
    let fieldColumns = 27
    let fieldRows = 27
    let fieldCenterColumn = (fieldOffsetColumn + fieldColumns) / 2
    let fieldCenterRow    = (fieldOffsetRow    + fieldRows)    / 2

    let fieldPositions =
        [for column = 0 to (fieldColumns-1) do
            for row = 0 to (fieldRows-1) do
                yield (fieldOffsetColumn + column, fieldOffsetRow + row)]
        |> List.map
            (fun p ->
                (p, p |> Position.add ((-fieldCenterColumn), (-fieldCenterRow))))

    //status bar (0,29,40,1)
    //minimap (30,?,9,9)
    //inventory (30,1,9,4)
    let inventoryOffsetX = 30
    let inventoryOffsetY = 1
    let inventoryColumns = GameState.inventoryColumns
    let inventoryRows = GameState.inventoryRows

    let fieldFrameCells =
        Map.empty
        |> Map.add (fieldFrameOffsetX                        , fieldFrameOffsetY)                      (RenderCell.makeFrameRenderCell 0xc9uy)
        |> Map.add (fieldFrameOffsetX + fieldFrameColumns - 1, fieldFrameOffsetY)                      (RenderCell.makeFrameRenderCell 0xbbuy)
        |> Map.add (fieldFrameOffsetX                        , fieldFrameOffsetY + fieldFrameRows - 1) (RenderCell.makeFrameRenderCell 0xc8uy)
        |> Map.add (fieldFrameOffsetX + fieldFrameColumns - 1, fieldFrameOffsetY + fieldFrameRows - 1) (RenderCell.makeFrameRenderCell 0xbcuy)
        |> RenderCell.hline   (RenderCell.makeFrameRenderCell 0xcduy) (fieldFrameOffsetX + 1                  , fieldFrameOffsetY)                    (fieldFrameColumns - 2)
        |> RenderCell.hline   (RenderCell.makeFrameRenderCell 0xcduy) (fieldFrameOffsetX + 1                  , fieldFrameOffsetY + fieldFrameRows-1) (fieldFrameColumns - 2)
        |> RenderCell.vline   (RenderCell.makeFrameRenderCell 0xbauy) (fieldFrameOffsetX                      , fieldFrameOffsetY + 1)                (fieldFrameRows    - 2)
        |> RenderCell.vline   (RenderCell.makeFrameRenderCell 0xbauy) (fieldFrameOffsetX + fieldFrameColumns-1, fieldFrameOffsetY + 1)                (fieldFrameRows    - 2)
        |> Map.add (panelFrameOffsetX                        , panelFrameOffsetY)                      (RenderCell.makeFrameRenderCell 0xc9uy)
        |> Map.add (panelFrameOffsetX + panelFrameColumns - 1, panelFrameOffsetY)                      (RenderCell.makeFrameRenderCell 0xbbuy)
        |> Map.add (panelFrameOffsetX                        , panelFrameOffsetY + panelFrameRows - 1) (RenderCell.makeFrameRenderCell 0xc8uy)
        |> Map.add (panelFrameOffsetX + panelFrameColumns - 1, panelFrameOffsetY + panelFrameRows - 1) (RenderCell.makeFrameRenderCell 0xbcuy)
        |> RenderCell.hline   (RenderCell.makeFrameRenderCell 0xcduy) (panelFrameOffsetX + 1                  , panelFrameOffsetY)                    (panelFrameColumns - 2)
        |> RenderCell.hline   (RenderCell.makeFrameRenderCell 0xcduy) (panelFrameOffsetX + 1                  , panelFrameOffsetY + panelFrameRows-1) (panelFrameColumns - 2)
        |> RenderCell.vline   (RenderCell.makeFrameRenderCell 0xbauy) (panelFrameOffsetX                      , panelFrameOffsetY + 1)                (panelFrameRows    - 2)
        |> RenderCell.vline   (RenderCell.makeFrameRenderCell 0xbauy) (panelFrameOffsetX + panelFrameColumns-1, panelFrameOffsetY + 1)                (panelFrameRows    - 2)

    let backBufferSize = (cellColumns * cellWidth,cellRows * cellHeight)


