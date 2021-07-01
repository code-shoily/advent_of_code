defmodule AdventOfCode.Helpers.FileWriter do
  @moduledoc false

  @input_dir Path.join(:code.priv_dir(:advent_of_code), "input_files")

  @type file_type :: :code | :data | :test

  @spec write(tuple) :: String.t()
  def write({year, day} = data) do
    code_content = EEx.eval_file("lib/helpers/templates/code.eex", day: day, year: year)
    test_content = EEx.eval_file("lib/helpers/templates/test.eex", day: day, year: year)

    data_file = path_for(data, :data) |> write_input_data(year, day)
    code_file = path_for(data, :code) |> write_unless_exists(code_content)
    test_file = path_for(data, :test) |> write_unless_exists(test_content)

    output({data_file, code_file, test_file})
  end

  @spec path_for({any, any}, file_type) :: Path.t()
  defp path_for({year, day}, :data), do: Path.join(@input_dir, "#{year}_#{day}.txt")
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

  @spec fetch_input_data(integer(), integer()) :: {:ok, String.t()} | nil
  def fetch_input_data(year, day) do
    HTTPoison.start()

    "https://adventofcode.com/#{year}/day/#{day}/input"
    |> HTTPoison.get([{"cookie", "session=#{System.get_env("COOKIE", "")}"}])
    |> then(fn response ->
      case response do
        {:ok, %HTTPoison.Response{body: body}} -> {:ok, body}
        _ -> nil
      end
    end)
  end

  @spec write_input_data(String.t(), integer(), integer()) :: String.t()
  def write_input_data(path, year, day) do
    with {:ok, data} = fetch_input_data(year, day),
         {:ok, file} = File.open(path, [:write]),
         :ok <- IO.write(file, data) do
      File.close(file)
    end
  end
end
