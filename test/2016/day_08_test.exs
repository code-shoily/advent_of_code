defmodule AdventOfCode.Y2016.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1608

  alias AdventOfCode.Y2016.Day08, as: Solution

  test "Year 2016, Day 8 run/1" do
    assert Solution.run() == {115, :ok}
  end
end
