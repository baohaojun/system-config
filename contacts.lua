#!/usr/bin/lua

local read_vcf, check_last_field
local base64, fix_contact

check_last_field = function (contact, field)
   if field:match("ENCODING=QUOTED%-PRINTABLE") then
      local data = contact[field]
      if not contact[field] then
         print("field is nil: " .. field)
      end

      contact[field] = contact[field]:gsub(
         "=+(%x%x)",
         function(h) return string.char(tonumber(h,16)) end )
   end
end

fix_contact = function (contact)

   for field, value in pairs(contact) do
      if field == "TELS" or field == "EMAILS"  or field == "FNWrench" then
         goto continue
      end
      check_last_field(contact, field)
      if (field:match("^FN;?")) then
         contact.FNWrench = contact[field]
      end
      ::continue::
   end
end

read_vcf = function (vcf_path)
   local contacts = {}
   local vcf_file, _, errno = io.open(vcf_path, "rb")
   if not vcf_file then
      error(_, errno)
   end

   local contact = {}
   contact.TELS = {}
   contact.EMAILS = {}
   contact.FNWrench = ""

   local last_field = ""
   local collecting_photo = false
   for line in vcf_file:lines() do
      if line:match("^END:VCARD") then
         contacts[#contacts + 1] = contact
         contact = {}
         contact.TELS = {}
         contact.EMAILS = {}
         contact.FNWrench = ""
      elseif line:match("^PHOTO;") then
         collecting_photo = true;
         if not line:match("^PHOTO;ENCODING=BASE64;") then
            error ("Photo is not base64 encoded")
         end

         local photo_type = line:gsub("^PHOTO;ENCODING=BASE64;", "")
         photo_type = photo_type:gsub(";.*", "")
         contact.photo_type = photo_type;

         line = line:gsub(".*:", "")
         line = line:gsub("%s+", "")

         contact.photo_data = line
      elseif collecting_photo then
         line = line:gsub("%s+", "")
         contact.photo_data = contact.photo_data .. line
         if line == "" then
            collecting_photo = false
         end
      else
         local field, data
         line = line:gsub("%s+", "")
         if line:match(":") then
            field = line:gsub(":.*", "")
            data = line:gsub(".*:", "")
            contact[field] = data
            last_field = field

            if (field:match("^TEL;?")) then
               contact.TELS[#contact.TELS + 1] = (contact[field]):gsub("%-", "")
            end

            if (field:match("^EMAIL;?")) then
               contact.EMAILS[#contact.EMAILS + 1] = (contact[field])
            end
         else
            contact[last_field] = contact[last_field] .. line
         end
      end
   end
   for i = 1, #contacts do
      fix_contact(contacts[i])
   end
   return contacts
end

if arg ~= nil then
   local arg1 = arg[1]
   arg = nil
   base64 = require'base64'
   vcfs = read_vcf(arg1)
   for vcf in ipairs(vcfs) do
      local tels = vcfs[vcf].TELS
      for i in ipairs(tels) do
         print(tels[i])
      end
   end
else
   base64 = require'base64'
   local M = {}
   M.read_vcf = read_vcf
   return M
end
