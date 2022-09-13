const HTTP_ADDR: &'static str = "0.0.0.0:1980";
const HTML_DATA: &'static str = "html/index.html";

pub async fn serve() -> Result<(), std::io::Error> {
    let mut app = tide::new();
    app.at("/").serve_file(HTML_DATA)?;
    app.listen(HTTP_ADDR).await?;
    Ok(())
}
