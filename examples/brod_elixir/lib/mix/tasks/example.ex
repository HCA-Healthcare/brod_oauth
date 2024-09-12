defmodule Mix.Tasks.Example do

  use Mix.Task

  @impl Mix.Task
  def run(_) do
    Example.test(:keycloak)
  end

end
