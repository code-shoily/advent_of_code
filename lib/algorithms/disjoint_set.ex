defmodule AdventOfCode.Algorithms.DisjointSet do
  @moduledoc """
  Implementation of a discjoint set data structure.

  More on this: https://en.wikipedia.org/wiki/Disjoint_sets
  """
  defstruct parents: %{}, ranks: %{}

  @type mapped_array() :: %{required(non_neg_integer()) => non_neg_integer()}
  @type value() :: non_neg_integer()
  @type t() :: %__MODULE__{
          parents: mapped_array(),
          ranks: mapped_array()
        }

  @doc """
  State of a disjoint set with initialized ranks and parents.

  ## Example

      iex> DisjointSet.new(0)
      %DisjointSet{}

      iex> DisjointSet.new(3)
      %DisjointSet{
        parents: %{0 => 0, 1 => 1, 2 => 2},
        ranks: %{0 => 1, 1 => 1, 2 => 1}
      }

  """
  @spec new(non_neg_integer() | List.t()) :: t()
  def new(0), do: %__MODULE__{}

  def new(size) when is_integer(size) do
    %__MODULE__{
      ranks: 0..(size - 1) |> Map.new(&{&1, 1}),
      parents: 0..(size - 1) |> Map.new(&{&1, &1})
    }
  end

  def new(lst) when is_list(lst) do
    %__MODULE__{
      ranks: lst |> Map.new(&{&1, 1}),
      parents: lst |> Map.new(&{&1, &1})
    }
  end

  @doc """
  Finds the parent of a given node. Returns `:error` if given a node that does
  not exist.

  ## Example

      iex> set = DisjointSet.new(4)
      iex> Enum.reduce(0..3, true, fn x, acc ->
      ...>   {value, _} = DisjointSet.find(set, x)
      ...>   value == x and acc
      ...> end)
      true

      iex> set = %DisjointSet{
      ...>  parents: %{0 => 1, 1 => 2, 2 => 3, 3 => 3, 4 => 1},
      ...>  ranks: %{0 => 1, 1 => 1, 2 => 1, 3 => 1, 4 => 1}
      ...> }
      iex> {3, new_set} = DisjointSet.find(set, 3)
      iex> new_set.parents == set.parents
      true
      iex> new_set.ranks == set.ranks
      true

      iex> set = %DisjointSet{
      ...>  parents: %{0 => 1, 1 => 2, 2 => 3, 3 => 3, 4 => 1},
      ...>  ranks: %{0 => 1, 1 => 1, 2 => 1, 3 => 1, 4 => 1}
      ...> }
      iex> {3, set} = DisjointSet.find(set, 0)
      iex> set.parents
      %{0 => 3, 1 => 3, 2 => 3, 3 => 3, 4 => 1}
      iex> set.ranks
      %{0 => 1, 1 => 1, 2 => 1, 3 => 1, 4 => 1}

    iex> DisjointSet.new(4) |> DisjointSet.find(100)
    :error

  """
  @spec find(t(), value()) :: {value(), t()} | :error
  def find(%__MODULE__{parents: parents} = disjoint_set, value) do
    case parents[value] do
      nil -> :error
      parent -> do_find(disjoint_set, [value], parent)
    end
  end

  defp do_find(%__MODULE__{parents: parents} = disjoint_set, path, value) do
    case parents[value] do
      ^value ->
        {value,
         Enum.reduce(path, disjoint_set, fn x, acc ->
           %{acc | parents: %{acc.parents | x => value}}
         end)}

      parent ->
        do_find(disjoint_set, [value | path], parent)
    end
  end

  @doc """
  Performs a union between two elements and returns the updated set. `:error` case is matched so that it fails
  in a piped flow.

  ## Example

      iex> set = DisjointSet.new(5)
      iex> set =
      ...>   set
      ...>   |> DisjointSet.union(0, 2)
      ...>   |> DisjointSet.union(4, 2)
      ...>   |> DisjointSet.union(3, 1)
      iex> set
      %DisjointSet{
        parents: %{0 => 0, 1 => 3, 2 => 0, 3 => 3, 4 => 0},
        ranks: %{0 => 2, 1 => 1, 2 => 1, 3 => 2, 4 => 1}
      }
      iex> DisjointSet.union(set, 3, 1) == set
      true

      iex> DisjointSet.new(1) |> DisjointSet.union(100, 200)
      :error

      iex> DisjointSet.union(:error, 100, 200)
      :error

  """
  @spec union(t() | :error, value(), value()) :: t() | :error
  def union(%__MODULE__{} = disjoint_set, a, b) do
    with {root_a, disjoint_set} <- find(disjoint_set, a),
         {root_b, disjoint_set} <- find(disjoint_set, b) do
      union_by_rank(disjoint_set, root_a, root_b)
    else
      _ -> :error
    end
  end

  def union(:error, _, _), do: :error

  @doc """
  Returns the connected components of a set of data. `:error` case is matched so that it fails
  in a piped flow.

  ## Example

      iex> DisjointSet.new([{0, 0}, {0, 1}, {0, 2}, {10, 11}, {10, 12}, {100, 200}])
      ...> |> DisjointSet.union({0, 0}, {0, 1})
      ...> |> DisjointSet.union({0, 1}, {0, 2})
      ...> |> DisjointSet.union({10, 11}, {10, 12})
      ...> |> DisjointSet.components()
      [MapSet.new([{0, 0}, {0, 1}, {0, 2}]), MapSet.new([{10, 11}, {10, 12}]), MapSet.new([{100, 200}])]

      iex> DisjointSet.new(10)
      ...> |> DisjointSet.union(20, 30)
      ...> |> DisjointSet.components()
      :error

  """
  @spec components(t() | :error) :: [[term()]]
  def components(%__MODULE__{parents: parents}) do
    parents
    |> Enum.group_by(&elem(&1, 1), fn {a, _} -> a end)
    |> Map.values()
    |> Enum.map(&Enum.into(&1, %MapSet{}))
  end

  def components(:error), do: :error

  defp union_by_rank(disjoint_set, parent, parent), do: disjoint_set

  defp union_by_rank(%__MODULE__{ranks: ranks} = disjoint_set, root_a, root_b) do
    case {ranks[root_a], ranks[root_b]} do
      {rank, rank} ->
        %{
          disjoint_set
          | parents: %{disjoint_set.parents | root_b => root_a},
            ranks: %{disjoint_set.ranks | root_a => rank + 1}
        }

      {rank_a, rank_b} when rank_a < rank_b ->
        %{disjoint_set | parents: %{disjoint_set.parents | root_a => root_b}}

      {rank_a, rank_b} when rank_a > rank_b ->
        %{disjoint_set | parents: %{disjoint_set.parents | root_b => root_a}}
    end
  end
end
