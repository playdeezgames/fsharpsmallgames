open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Game1(backBufferSize: int*int) as this=
    inherit Game()

    do
        this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)
    let mutable vertexBuffer : VertexBuffer = Unchecked.defaultof<VertexBuffer>
    let mutable world = Matrix.CreateTranslation(0.0f,0.0f,0.0f)
    let view = Matrix.CreateLookAt(new Vector3(0.0f,0.0f,3.0f), new Vector3(0.0f,0.0f,0.0f), new Vector3(0.0f,1.0f,0.0f))
    let projection = Matrix.CreatePerspectiveFieldOfView(MathHelper.ToRadians(60.0f), 1.25f, 0.01f, 100.0f)
    let mutable basicEffect : BasicEffect = Unchecked.defaultof<BasicEffect>

    override this.Initialize() =
        graphics.PreferredBackBufferWidth  <- backBufferSize |> fst
        graphics.PreferredBackBufferHeight <- backBufferSize |> snd
        graphics.ApplyChanges()
        base.Initialize()
    override this.LoadContent() =
        
        basicEffect <- new BasicEffect(this.GraphicsDevice)
        basicEffect.View <- view
        basicEffect.Projection <- projection
        basicEffect.VertexColorEnabled <- true

        vertexBuffer<- new VertexBuffer(this.GraphicsDevice, typedefof<VertexPositionColor> , 3, BufferUsage.WriteOnly)

        let vertices = 
            [new VertexPositionColor(new Vector3(0.0f,0.866f,0.0f), Color.Red);
            new VertexPositionColor(new Vector3(1.0f,-0.5f,0.0f), Color.Green);
            new VertexPositionColor(new Vector3(-1.0f,-0.5f,0.0f), Color.Blue)]
            |> List.toArray

        vertexBuffer.SetData<VertexPositionColor>(vertices)

        let mutable rasterizerState = new RasterizerState()
        rasterizerState.CullMode <- CullMode.None
        this.GraphicsDevice.RasterizerState <- rasterizerState


    override this.Update delta =
        world <- Matrix.Multiply(world, Matrix.CreateRotationY(MathHelper.ToRadians(5.0f)))
        base.Update delta

    override this.Draw delta =
        Color.Black
        |> this.GraphicsDevice.Clear

        basicEffect.World <- world

        this.GraphicsDevice.SetVertexBuffer(vertexBuffer)

        basicEffect.CurrentTechnique.Passes
        |> Seq.iter
            (fun pass -> 
                pass.Apply()
                this.GraphicsDevice.DrawPrimitives(PrimitiveType.TriangleList, 0, 1))

        base.Draw delta
    
[<EntryPoint>]
let main _ = 
    use game = new Game1((800,600))
    game.Run()
    0
