defmodule AdventOfCode do
  @moduledoc """
  Module that solves problem given year and day.
  """
  @latest_year 2024

  @type year() :: 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024
  @type day() :: pos_integer()

  @doc """
  Returns the latest year solved.
  """
  def get_latest_year, do: @latest_year

  @doc """
  Solves for `year` and `day`.
  """
  @spec solve(year(), day()) :: {any(), any()}
  def solve(year, day, rust? \\ false) do
    solution =
      :timer.tc(fn ->
        case rust? do
          true ->
            {_, result} = AdventOfCode.RustNif.solve(year, day)
            result

          false ->
            Module.concat([AdventOfCode, get_year_module(year), get_day_module(day)]).run()
        end
      end)

    {:ok, solution}
  rescue
    _ in FunctionClauseError -> {:error, :invalid_args}
    _ in UndefinedFunctionError -> {:error, :not_yet_solved}
  end

  defp get_year_module(year) when year >= 2015 and year <= @latest_year do
    "Y" <> Integer.to_string(year)
  end

  defp get_day_module(day) when day >= 1 and day <= 25 do
    day
    |> Integer.to_string()
    |> String.pad_leading(2, "0")
    |> then(fn day -> "Day" <> day end)
  end
end
