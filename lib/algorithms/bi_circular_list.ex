defmodule AdventOfCode.Algorithms.BiCircularList do
  @moduledoc """
  A Circular + BiDirectional List.
  """
  alias AdventOfCode.Algorithms.BiCircularList

  @type t :: %BiCircularList{visited: [any()], current: any(), upcoming: [any()]}

  defstruct visited: [], current: nil, upcoming: []

  @doc """
  Checks if the list is empty.

  ## Example

      iex> %BiCircularList{} |> BiCircularList.empty?()
      true

      iex> %BiCircularList{current: 10} |> BiCircularList.empty?()
      false

  """
  @spec empty?(BiCircularList.t()) :: boolean()
  def empty?(%BiCircularList{current: current}), do: is_nil(current)

  @doc """
  Adds `value` AFTER the current value and sets the cursor as current.

  ## Example

    iex> BiCircularList.insert(%BiCircularList{}, 10)
    %BiCircularList{visited: [], current: 10, upcoming: []}

    iex>  %BiCircularList{visited: [10, 20], current: 30, upcoming: [-40]}
    ...>  |> BiCircularList.insert(-10)
    ...>  |> BiCircularList.insert(-20)
    %BiCircularList{visited: [-10, 30, 10, 20], current: -20, upcoming: [-40]}

  """
  @spec insert(BiCircularList.t(), any()) :: BiCircularList.t()
  def insert(%{current: nil} = bcl, value) when not is_nil(value) do
    %BiCircularList{bcl | current: value}
  end

  def insert(%BiCircularList{visited: visited, current: current} = bcl, value) do
    %BiCircularList{bcl | visited: [current | visited], current: value}
  end

  @doc """
  Removes the current element and returns the new list with current element as
  the next one.

  ## Example
      iex> empty_bcl = %BiCircularList{}
      iex> BiCircularList.pop(empty_bcl)
      :error

      iex> bcl = %BiCircularList{current: 1}
      iex> BiCircularList.pop(bcl)
      {1, %BiCircularList{}}

      iex> bcl = %BiCircularList{visited: [1, 2, 3], current: 4}
      iex> BiCircularList.pop(bcl)
      {4, %BiCircularList{visited: [], current: 3, upcoming: [2, 1]}}

      iex> bcl = %BiCircularList{current: 5, upcoming: [6, 7, 8]}
      iex> BiCircularList.pop(bcl)
      {5, %BiCircularList{current: 6, upcoming: [7, 8]}}

      iex> bcl = %BiCircularList{visited: [1, 2], current: 3, upcoming: [5, 6]}
      iex> BiCircularList.pop(bcl)
      {3, %BiCircularList{visited: [1, 2], current: 5, upcoming: [6]}}
  """
  @spec pop(BiCircularList.t()) :: {any(), BiCircularList.t()} | :error
  def pop(%BiCircularList{current: nil}), do: :error

  def pop(%BiCircularList{visited: [], current: value, upcoming: []}) do
    {value, %BiCircularList{}}
  end

  def pop(%BiCircularList{visited: visited, current: value, upcoming: []}) do
    [head | rest] = Enum.reverse(visited)
    {value, %BiCircularList{visited: [], current: head, upcoming: rest}}
  end

  def pop(%BiCircularList{visited: visited, current: value, upcoming: [next | upcoming]}) do
    {value, %BiCircularList{visited: visited, current: next, upcoming: upcoming}}
  end

  @doc """
  Move cursor to the next. Wraps back to first if end of list is reached.

  ## Example

      iex> BiCircularList.next(%BiCircularList{})
      :error

      iex> BiCircularList.next(%BiCircularList{current: :single})
      %BiCircularList{current: :single}

      iex> %BiCircularList{visited: [:before], current: 20, upcoming: [:after]}
      ...> |> BiCircularList.next()
      ...> |> BiCircularList.next()
      ...> |> BiCircularList.next()
      ...> |> BiCircularList.next()
      %BiCircularList{visited: [20, :before], current: :after, upcoming: []}

      iex> bcl = BiCircularList.next(%BiCircularList{current: 0, upcoming: [10, 20, 30]})
      %BiCircularList{visited: [0], current: 10, upcoming: [20, 30]}
      iex> bcl = BiCircularList.next(bcl)
      %BiCircularList{visited: [10, 0], current: 20, upcoming: [30]}
      iex> bcl = BiCircularList.next(bcl)
      %BiCircularList{visited: [20, 10, 0], current: 30, upcoming: []}
      iex> BiCircularList.next(bcl)
      %BiCircularList{visited: [], current: 0, upcoming: [10, 20, 30]}
  """
  @spec next(BiCircularList.t()) :: BiCircularList.t() | :error
  def next(%BiCircularList{current: nil}), do: :error
  def next(%BiCircularList{visited: [], current: _, upcoming: []} = bcl), do: bcl

  def next(%BiCircularList{visited: visited, current: current, upcoming: []}) do
    [head | upcoming] = Enum.reverse([current | visited])
    %BiCircularList{visited: [], current: head, upcoming: upcoming}
  end

  def next(%BiCircularList{visited: visited, current: current, upcoming: [upnext | rest]}) do
    %BiCircularList{visited: [current | visited], current: upnext, upcoming: rest}
  end

  @doc """
  Goes backward.

  ## Example
      iex> BiCircularList.previous(%BiCircularList{})
      :error

      iex> BiCircularList.previous(%BiCircularList{current: :single})
      %BiCircularList{current: :single}

      iex> bcl = BiCircularList.previous(%BiCircularList{current: 0, upcoming: [10, 20, 30]})
      %BiCircularList{visited: [20, 10, 0], current: 30, upcoming: []}
      iex> bcl = BiCircularList.previous(bcl)
      %BiCircularList{visited: [10, 0], current: 20, upcoming: [30]}
      iex> bcl = BiCircularList.previous(bcl)
      %BiCircularList{visited: [0], current: 10, upcoming: [20, 30]}
      iex> BiCircularList.previous(bcl)
      %BiCircularList{visited: [], current: 0, upcoming: [10, 20, 30]}
  """
  @spec previous(BiCircularList.t()) :: BiCircularList.t() | :error
  def previous(%BiCircularList{current: nil}), do: :error
  def previous(%BiCircularList{visited: [], current: _, upcoming: []} = bcl), do: bcl

  def previous(%BiCircularList{visited: [], current: value, upcoming: upcoming}) do
    [last | rest] = Enum.reverse([value | upcoming])
    %BiCircularList{visited: rest, current: last, upcoming: []}
  end

  def previous(%BiCircularList{visited: [first | visited], current: value, upcoming: upcoming}) do
    %BiCircularList{visited: visited, current: first, upcoming: [value | upcoming]}
  end
end
