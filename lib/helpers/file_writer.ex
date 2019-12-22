defmodule AdventOfCode.Helpers.FileWriter do
  @type file_type :: :code | :data

  @spec write(tuple) :: String.t()
  def write({year, day} = data) do
    code_content = EEx.eval_file("lib/helpers/templates/code.eex", day: day, year: year)
    data_file = path_for(data, :data) |> write_unless_exists()
    code_file = path_for(data, :code) |> write_unless_exists(code_content)

    output({data_file, code_file})
  end

  @spec path_for(tuple, file_type) :: Path.t()
  defp path_for({year, day}, :data), do: "lib/data/inputs/#{year}_#{day}.txt"
  defp path_for({year, day}, :code), do: "lib/#{year}/day_#{day}.ex"

  @spec write_unless_exists(String.t(), iodata) :: :ok | {:error, File.posix()}
  defp write_unless_exists(path, content \\ "") do
    unless File.exists?(path), do: File.write(path, content)
  end

  @spec output(tuple) :: String.t()
  defp output({:ok, :ok}), do: "Both files created"
  defp output({:ok, _}), do: "Data file created. Error writing code file"
  defp output({_, :ok}), do: "Code file created, Error writing data file"
  defp output({_, _}), do: "No file could be created"
end
