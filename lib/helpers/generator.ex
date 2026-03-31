defmodule AdventOfCode.Helpers.Generator do
  @moduledoc """
  Module responsible for writing the generated code.
  """

  @code_template "lib/helpers/templates/code.eex"
  @test_template "lib/helpers/templates/test.eex"

  @doc """
  Generates the necessary artifacts for solving a day's problem.

  Given {year, day} as input, it creates the following-

  - Input file fetched from advent of code site and saved at `priv/input_files/` as `<year>_<day>.txt` file.
  - Code file containing boilerplate code saved at `lib/<year>/day_<day>.ex` file
  - Test cases containing default tests for `run_1` and `run_2` at `test/<year>/day_<day>_test.exs` file
  """
  def run({year, day}) do
    padded_day = zero_padded(day)

    # Write the input data at `priv/input_files`
    input_path = "priv/input_files/#{year}_#{day}.txt"
    input_result = create_input_file(input_path, year, day)

    # Write code files at `lib/<year>/day_<day>.ex`
    code_content =
      @code_template
      |> EEx.eval_file(day: day, year: year, title: get_title(year, day))

    code_path = "lib/#{year}/day_#{padded_day}.ex"
    code_result = create_file(code_path, code_content)

    # Write test files at `test/<year>/day_<year>_test.exs`
    test_content =
      @test_template
      |> EEx.eval_file(day: day, year: year)

    test_path = "test/#{year}/day_#{padded_day}_test.exs"
    test_result = create_file(test_path, test_content)

    [input_result, code_result, test_result]
  end

  defp create_file(path, content) do
    if File.exists?(path) do
      {:exists, path}
    else
      path |> Path.dirname() |> File.mkdir_p!()

      case File.write(path, content) do
        :ok -> {:ok, path}
        {:error, reason} -> {:error, path, reason}
      end
    end
  end

  defp fetch_cookie(year, day) do
    session_key = System.get_env("AOC_SESSION_KEY")

    if is_nil(session_key) or session_key == "" do
      {:error, "AOC_SESSION_KEY environment variable is not set"}
    else
      HTTPoison.start()

      "https://adventofcode.com/#{year}/day/#{day}/input"
      |> HTTPoison.get([{"cookie", "session=#{session_key}"}])
      |> then(fn response ->
        case response do
          {:ok, %HTTPoison.Response{status_code: 200, body: body}} -> {:ok, body}
          {:ok, %HTTPoison.Response{status_code: status}} -> {:error, "Received HTTP #{status}"}
          {:error, %HTTPoison.Error{reason: reason}} -> {:error, reason}
          _ -> {:error, "unknown error"}
        end
      end)
    end
  end

  defp create_input_file(path, year, day) do
    if File.exists?(path) do
      {:exists, path}
    else
      path |> Path.dirname() |> File.mkdir_p!()

      case fetch_cookie(year, day) do
        {:ok, data} ->
          case File.write(path, data) do
            :ok -> {:ok, path}
            {:error, reason} -> {:error, path, reason}
          end

        {:error, reason} ->
          {:error, path, reason}
      end
    end
  end

  defp get_title(year, day) do
    HTTPoison.start()

    "https://adventofcode.com/#{year}/day/#{day}"
    |> HTTPoison.get()
    |> then(fn response ->
      case response do
        {:ok, %HTTPoison.Response{body: body}} -> body
        _ -> nil
      end
    end)
    |> Floki.parse_document!()
    |> Floki.find("h2")
    |> then(fn [{"h2", [], [title | _]}] -> title end)
  rescue
    _ -> ""
  end

  defp zero_padded(day), do: day |> to_string() |> String.pad_leading(2, "0")
end
