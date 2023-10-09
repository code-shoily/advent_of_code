defmodule AdventOfCode.Helpers.Meta do
  def get_solution_count(year, day) do
    year
    |> ast_at_path(day)
    |> get_solution_count(year, day)
  end

  def get_solution_count(nil, _, _), do: 0

  def get_solution_count({:ok, {:defmodule, _, module_content}}, year, day) do
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

  defp ast_at_path(year, day) do
    case File.read("test/#{year}/day_#{padded(day)}_test.exs") do
      {:ok, content} -> content |> Code.string_to_quoted()
      _ -> nil
    end
  end

  defp padded(day), do: String.pad_leading(to_string(day), 2, "0")
end
