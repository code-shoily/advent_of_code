defmodule AdventOfCode.Helpers.FileWriter do
  @type file_type :: :code | :data | :test

  @spec write(tuple) :: String.t()
  def write({year, day} = data) do
    code_content = EEx.eval_file("lib/helpers/templates/code.eex", day: day, year: year)
    test_content = EEx.eval_file("lib/helpers/templates/test.eex", day: day, year: year)

    data_file = path_for(data, :data) |> write_unless_exists()
    code_file = path_for(data, :code) |> write_unless_exists(code_content)
    test_file = path_for(data, :test) |> write_unless_exists(test_content)

    output({data_file, code_file, test_file})
  end

  @spec path_for({any, any}, file_type) :: Path.t()
  defp path_for({year, day}, :data), do: "lib/data/inputs/#{year}_#{day}.txt"
  defp path_for({year, day}, :code), do: "lib/#{year}/day_#{day}.ex"
  defp path_for({year, day}, :test), do: "test/#{year}/day_#{day}_test.exs"

  @spec write_unless_exists(String.t(), iodata) :: :ok | {:error, File.posix()}
  defp write_unless_exists(path, content \\ "") do
    unless File.exists?(path), do: File.write(path, content)
  end

  @spec output(tuple) :: String.t()
  defp output({data, code, test}) do
    report = fn
      :ok -> "Yes"
      _ -> "No"
    end

    "Data: #{report.(data)}, Code: #{report.(code)}, Test: #{report.(test)}\n"
  end
end
