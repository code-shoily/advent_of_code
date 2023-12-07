defmodule AdventOfCode.Y2023.Day07Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2307

  alias AdventOfCode.Y2023.Day07, as: Solution

  test "Year 2023, Day 7 run/1" do
    assert Solution.run() == {255_048_101, 253_718_286}
  end
end
