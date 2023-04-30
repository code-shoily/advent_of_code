defmodule AdventOfCode.Y2020.Day18Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2018

  alias AdventOfCode.Y2020.Day18, as: Solution

  test "Year 2020, Day 18" do
    assert Solution.run() == {50_956_598_240_016, 535_809_575_344_339}
  end
end
