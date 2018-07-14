defmodule BookShopProject do
  use Mix.Project

  def project do
    [
      app: :book_shop,
      version: "1.0.0",
      elixirc_paths: ["src_ex"],
      deps: deps()
    ]
  end

  defp deps do
    [
        {:dialyxir, "~> 0.5", only: [:dev]},
        {:monad, "~> 1.0.5"}
    ]
  end

end