defmodule AdventOfCode.Helpers.Meta do
  @moduledoc """
  Returns the info of the year. This returns the important links and info
  of all the days completed.
  """
  alias AdventOfCode.Helpers.Transformers

  @current_year 2023

  def solutions_summary do
    for year <- 2015..@current_year, into: %{} do
      {year, get_info(year, true)}
    end
  end

  def get_info(year, as_map \\ false) do
    daywise_summary =
      for day <- 1..25 do
        get_info_for_day(year, day)
      end
      |> Enum.reject(&is_nil/1)
      |> then(fn data -> (as_map && Map.new(data)) || data end)

    stars_completed =
      daywise_summary
      |> Enum.map(fn {_, %{count: count}} -> count end)
      |> Enum.sum()

    tag_summary = daywise_summary |> group_by_tags()
    difficulty_summary = daywise_summary |> group_by_difficulty()

    %{
      completed: stars_completed,
      link: "http://adventofcode/#{year}",
      test: "/test/#{year}/",
      summary: daywise_summary,
      tag_summary: tag_summary,
      difficulty_summary: difficulty_summary
    }
  end

  defp group_by_tags(summary) do
    summary
    |> Enum.map(fn {_, a} -> a end)
    |> Enum.flat_map(fn line ->
      line[:tags]
      |> Enum.map(fn tag ->
        {tag, line}
      end)
    end)
    |> Enum.group_by(fn {tag, _} -> tag end, fn {_, data} -> {data.day, data} end)
  end

  defp group_by_difficulty(summary) do
    summary
    |> Enum.map(fn {_, a} -> a end)
    |> Enum.group_by(& &1.difficulty, fn data -> {data.day, data} end)
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
        %{title: title, difficulty: difficulty, tags: tags, extra: extra} =
          get_metadata(year, day)

        {day,
         %{
           year: year,
           day: day,
           link: "https://adventofcode.com/#{year}/day/#{day}",
           solution: "/lib/#{year}/day_#{padded(day)}.ex",
           test: "/test/#{year}/day_#{padded(day)}_test.exs",
           title: title,
           difficulty: difficulty,
           tags: tags,
           extra: extra,
           count: solution_count
         }}
    end
  end

  def get_metadata(year, day) do
    "Elixir.AdventOfCode.Y#{year}.Day#{padded(day)}"
    |> String.to_existing_atom()
    |> Code.fetch_docs()
    |> elem(4)
    |> Map.fetch!("en")
    |> Transformers.lines()
    |> then(fn
      [title, _, difficulty, tags | []] ->
        {format_title(title), format_difficulty(difficulty), format_tags(tags), "N/A"}

      [title, _, difficulty, tags | extra] ->
        {format_title(title), format_difficulty(difficulty), format_tags(tags),
         format_extra(extra)}

      [title, _ | _] ->
        {format_title(title), "N/A", [], "N/A"}
    end)
    |> then(fn {title, difficulty, tags, extra} ->
      %{title: title, difficulty: difficulty, tags: tags, extra: extra}
    end)
  end

  defp format_title(padded_title) do
    ~r/--- Day (\d+): (?<name>.+) ---/
    |> Regex.named_captures(padded_title)
    |> Map.fetch!("name")
  end

  defp format_difficulty("Difficulty:" <> difficulty), do: String.trim(difficulty)
  defp format_tags("Tags:" <> tags), do: tags |> String.trim() |> String.split(" ", trim: true)
  defp format_extra(extra), do: Enum.join(extra, " ")

  defp get_solution_count(year, day) do
    year
    |> ast_at_test_path(day)
    |> get_solution_count(year, day)
  end

  defp get_solution_count(nil, _, _), do: 0

  defp get_solution_count({:ok, {:defmodule, _, module_content}}, year, day) do
    with [_, [{:do, do_block}]] <- module_content,
         {:__block__, _, cases} <- do_block do
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
