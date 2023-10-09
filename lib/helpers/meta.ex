defmodule AdventOfCode.Helpers.Meta do
  @doc """
  Returns the info of the year. This returns the important links and info
  of all the days completed.
  """
  def get_info(year) do
    daywise_summary =
      for day <- 1..25 do
        get_info_for_day(year, day)
      end
      |> Enum.reject(&is_nil/1)

    stars_completed =
      daywise_summary
      |> Enum.map(fn {_, %{count: count}} -> count end)
      |> Enum.sum()

    %{
      completed: stars_completed,
      link: "http://adventofcode/#{year}",
      test: "/test/#{year}/",
      summary: daywise_summary
    }
  end

  @doc """
  Returns the information for problem year/day. If this was not attempted then
  `nil` is returned. Otherwise a summary of links, solution status, title etc are
  returned.
  """
  def get_info_for_day(year, day) do
    case get_solution_count(year, day) do
      0 ->
        nil

      solution_count ->
        {day,
         %{
           year: year,
           day: day,
           link: "https://adventofcode.com/#{year}/day/#{day}",
           solution: "/lib/#{year}/day_#{padded(day)}.ex",
           test: "/test/#{year}/day_#{padded(day)}_test.exs",
           title: title(year, day),
           count: solution_count
         }}
    end
  end

  defp title(year, day) do
    "Elixir.AdventOfCode.Y#{year}.Day#{padded(day)}"
    |> String.to_existing_atom()
    |> Code.fetch_docs()
    |> elem(4)
    |> Map.fetch!("en")
    |> String.split("\n")
    |> hd()
    |> then(fn padded_title ->
      ~r/--- Day (\d+): (?<name>.+) ---/
      |> Regex.named_captures(padded_title)
      |> Map.fetch!("name")
    end)
  end

  defp get_solution_count(year, day) do
    year
    |> ast_at_test_path(day)
    |> get_solution_count(year, day)
  end

  defp get_solution_count(nil, _, _), do: 0

  defp get_solution_count({:ok, {:defmodule, _, module_content}}, year, day) do
    with [_, [{:do, do_block}]] <- module_content,
         {:__block__, _, cases} = do_block do
      cases
      |> Enum.filter(&(elem(&1, 0) == :test))
      |> Enum.map(&elem(&1, 2))
      |> Enum.filter(fn [title | _] ->
        title
        |> String.downcase()
        |> String.starts_with?("year #{year}, day #{day}")
      end)
      |> Enum.map(fn [_, [{:do, {_, _, [{:==, _, body}]}}]] ->
        List.last(body)
      end)
      |> then(fn
        [] -> -1
        [{:todo, _}, {:todo, _}] -> 0
        [{nil, nil}] -> 0
        [{_, nil}] -> 1
        [{_, {:todo, _}}] -> 1
        [_, {:todo, _}] -> 1
        [_, nil] -> 1
        [_, _] -> 2
        [{_, _}] -> 2
      end)
    end
  end

  defp ast_at_test_path(year, day) do
    case File.read("test/#{year}/day_#{padded(day)}_test.exs") do
      {:ok, content} -> content |> Code.string_to_quoted()
      _ -> nil
    end
  end

  defp padded(day), do: String.pad_leading(to_string(day), 2, "0")
end
