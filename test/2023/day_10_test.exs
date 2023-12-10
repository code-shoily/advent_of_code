defmodule AdventOfCode.Y2023.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2310

  alias AdventOfCode.Y2023.Day10, as: Solution

  test "Year 2023, Day 10 run/1" do
    assert Solution.run() == {7107, 258}
  end
end
