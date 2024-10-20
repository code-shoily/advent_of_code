defmodule AdventOfCode.Y2022.Day10Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2210

  alias AdventOfCode.Y2022.Day10, as: Solution

  test "Year 2022, Day 10 run/1" do
    assert Solution.run() == {11_820, :ok}
  end
end
