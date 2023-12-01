defmodule AdventOfCode.Y2023.Day02Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2302

  alias AdventOfCode.Y2023.Day02, as: Solution

  test "Year 2023, Day 2 run/1" do
    assert Solution.run() == {2085, 79_315}
  end
end
