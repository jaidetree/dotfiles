-- Load custom tree-sitter grammar for org filetype
require('orgmode').setup_ts_grammar()

require('orgmode').setup({
    org_agenda_files = {'~/org/*', '~/org/**/*'},
    org_default_notes_file = '~/org/notes.org',
})

local org_mappings = require('orgmode.org.mappings')

-- This function will be used instead of treesitter to find links
local is_link = function ()
    local is_link = org_mappings._get_link_under_cursor()
    if is_link == nil then
       return nil
    end

    return true
end

local type_to_action = {
    [is_link] = "org_mappings.open_at_point",
    timestamp = "org_mappings.change_date",
    headline = "org_mappings.todo_next_state",
    listitem = "org_mappings.toggle_checkbox",
    list = "org_mappings.toggle_checkbox",
    _default = "org_mappings.open_at_point"
}

local function get_action_from_type()
    local ts_utils = require('nvim-treesitter.ts_utils')
    local cur_node = ts_utils.get_node_at_cursor()
    local cur_row = cur_node:range()

    while cur_node ~= nil do
        local nodetype = cur_node:type()

        for identifier, action in pairs(type_to_action) do
            if type(identifier) == "function" then
                if identifier() ~= nil then
                    return action
                end
            elseif nodetype == identifier and identifier ~= "_default" then
                return action
            end
        end

        cur_node = cur_node:parent()
        if cur_node == nil then
            break
        elseif cur_node:range() ~= cur_row then
            break
        end
    end

    return type_to_action._default
end

local function toggle_org_item()
    local org = require('orgmode')

    local action = get_action_from_type()

    if action ~= nil then
        org.action(action)
    end
end

local query = vim.treesitter.query
local files = nil

local fix_indentation = function (str, min_spaces)
    if min_spaces == nil then
        min_spaces = 1000
    end

    for spaces in string.gmatch(str, "\n([ \t]+)") do
        if min_spaces > #spaces then
            min_spaces = #spaces
        end
    end

    local pattern = "\n"
    pattern = pattern .. string.rep("[ \t]", min_spaces)

    local indented = string.gsub(str, pattern, "\n")
    return indented
end

local save_into_files = function ()
    local out = "Exported into files: "
    for filename, code_blocks in pairs(files) do
        -- Without expanding the file will be nil
        local file = io.open(vim.fn.expand(filename), "w+")
        io.output(file)
        for _, block in ipairs(code_blocks) do
            io.write(block .. "\n\n")
        end
        io.close(file)
        out = out .. filename .. " "
    end
    print(out)
end

-- necessary for recursion in this case
local process_node = nil
process_node = function (node, cur_file)
    if node == nil then
        return
    end

    for subnode in node:iter_children() do
        local t = subnode:type()
        -- If the node is a block, try to tangle if necessary
        if t == "block" then
            if cur_file ~= nil then
                if files[cur_file] == nil then
                    files[cur_file] = {}
                end

                for block_prop in subnode:iter_children() do
                    if block_prop:type() == "contents" then
                        local _, col = block_prop:range()
                        table.insert(files[cur_file], fix_indentation(query.get_node_text(block_prop, 0), col))
                    end
                end
            end
        -- It was necessary to start from property_drawer, in order to
        -- pass "cur_file" to the node inside the section body
        elseif t == "property_drawer" then
            for drawer_child in subnode:iter_children() do
                if drawer_child:type() == "property" then
                    local is_tangle = false
                    -- Look for the property name and value
                    for prop_part in drawer_child:iter_children() do
                        local prop_type = prop_part:type()
                        local prop_text = query.get_node_text(prop_part, 0)
                        if prop_type == "expr" and prop_text == "TANGLE" then
                            is_tangle = true
                        elseif prop_type == "value" and is_tangle then
                            cur_file = prop_text
                            is_tangle = false
                        end
                    end
                end
            end
        else
            process_node(subnode, cur_file)
        end
    end
end

local tangle = function ()
    files = {}

    local language_tree = vim.treesitter.get_parser(0)
    local syntax_tree = language_tree:parse()
    local root = syntax_tree[1]:root()

    process_node(root, nil)
    save_into_files()
end

vim.api.nvim_create_autocmd("FileType", {
    pattern = "org",
    callback = function()
        vim.api.nvim_buf_set_keymap( 0, 'n', "<cr>", "", {
           callback = function ()
               toggle_org_item()
           end,
           noremap = true
        })

        vim.api.nvim_buf_set_keymap( 0, 'n', "<leader>oxt", "", {
            callback = function ()
                tangle()
            end,
            desc= "tangle",
            noremap = true
        })
    end
})

