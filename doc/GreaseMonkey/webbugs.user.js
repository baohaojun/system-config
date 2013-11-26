// ==UserScript==
// @name Web Bug Detector
// @namespace http://diveintomark.org/projects/greasemonkey/
// @description make web bugs visible
// @include *
// ==/UserScript==

var snapImages = document.evaluate("//img[@width='1'][@height='1']",
                                   document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null);
for (var i = snapImages.snapshotLength - 1; i >= 0; i--) {
  var elmImage = snapImages.snapshotItem(i);
  var urlSrc = elmImage.src;
  var urlHost = urlSrc.replace(/^(.*?):\/\/(.*?)\/(.*)$/, "$2");
  if (urlHost == window.location.host) continue;
  elmImage.width = '80';
  elmImage.height = '80';
  elmImage.title = 'Web bug detected! src="' + elmImage.src + '"';
  elmImage.src =
    'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFAAAABQCAYAAACOEfKtAA' +
    'AABHNCSVQICAgIfAhkiAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPB' +
    'oAAAv3SURBVHic7Zx5kFxVGcV%2FvWVmemaSSSYxiRkSgglECJK4gAtSEjQFLmjEUhEV' +
    'CxUs1NJyK5WScisKFbHUwo0q10jQABZoEJBEtgSdArNHkhCyTTKZzNo9S3dm6faPc2%2' +
    'FepNP9Xvfrnu5J2afqVXq6X7%2F3vXO%2F%2By3n3g5UUUUVVVRRRRVVVFFFFVVU4Y4p' +
    'lTbgTEUAmA%2F8CohU2JYzEtOBbwKDQH1lTTkVwQm%2Bfk0J7hEG5gCrgAFgYbFGlRIT' +
    'TWAT0EJx064BuMBc52nzetJgognsB6YBF%2BIvAQSAZuAa4F9AG4qFkwYTTWACxa3lwP' +
    'UU7olRYCbwduAp4Ih5P1wqA4vFRBOYBnqAHciLPl%2FgPRuBDwLDwEbgkHmdLq2Z%2Fh' +
    'Eqwz1GgVqgA7gT6AOey%2BN7ITRdvwfcAzxh3gsC2yfCUD%2BYaA8EEdgN7AceQuVISx' +
    '7fiwJXI2%2F7E5AETgC7mUQeWA4CAWLI89airPo1RJAbpgHXAr9HYaAfxdS%2BCbDPNw' +

    '%2FlInAM6ERJ4B%2FADbiXI0FE4CvQdE8gApPASIlsCqAYeyHwfeTlBVcK5SIQREAMuB' +
    'dNwVvJ3VXUAPOQlx5HU3cMDUB3kXZMAWYAVwG%2FBR43f%2B8C7qPASqGcBI4CXcBLKC' +
    'm8A1iR49wpwNko41rPA4ijsqhQBFHoaAE%2BhWbBPaizuRGFiVZgPbCaAsqkctdT%2FS' +
    'ie%2FRl4M%2FAdYAOnk1KPHjaOiB%2F2eb8aRNxS4L2oJKoFHjT3Pgr0mnvMQwkqiUSL' +
    'j5NHsnIjcKq5QAjFiySaRinzr59MOIySQBfwU%2BBHwGXA3zPOq0MExsy9Rgu4RxANwJ' +
    'tQFn8bsMhc6yHgAeAYGshONEgBc5%2F5wAHkmZ8xNrrCjcAw8DIUhy4zRtSah6vDSQwd' +
    'wH9R%2FEjl8YBxROJ2c7wfTSlLUgBN4Zeb80bNvbxQj8haBaxEAgTIy34OPILiaa%2Bx' +
    '2w6ORZuxfz5KVH0owbjWnG4E9iCPmYG6gB3m%2FSB6yFpE8BzgQ8CXgc%2BhntWNyCQw' +
    'ZB5gB3Almma2PAkjr59rHvSE2wOY714D%2FBhlbtAgPYPEh6fMtftwiMvm0SmUpMZQ%2' +
    'FE2i7B%2FAZbZ5xcABFJ%2B6jKENyPsiiMBe4EXg38DFaHqsA25BnpnrxnGk1GwFPgxc' +
    'DvxlnE1hROBe3ONfFHgdIi9hrrEVJapBlLF7EHmDeHtyGmhHg3YOiqFFEWgvOmSO48gD' +
    'a8wRRVOnFnjSGH6TeYjbgbvRIGRiyDzQduQNq1AcTBqbapEK00vuui%2BCptudxpavoM' +
    'GM4xTu4zN4Iegxdi1AuWAiiveTCKOC9CykulwF%2FBARtwf4pPk8E03A61H91QHMMu9P' +
    'R3ExjfrgpizfDSLy7jbn3Y48caE5v1Q9fhQls0CuE0pRB46ikW4DDiIv%2FBtSXoZRSd' +
    'ABfCPDEBsLd6FYusLYYz0LHE%2FIxEwUO28AHkZlSSfKrn3kl3TywZC5bs5lhFKrMQlz' +
    '0zHzuhWRsRDFuQGUZNLmnCZExuWI7EdRnL0Seee9wDZOJXEqcBHwCxSvbkGZ9jDeCccP' +
    'bBlVVgGjGTgPuAR4F%2FAYDmmfwBm4eebzFIqvc1C5tNac%2Fx4U3yxqgVcjr%2B0Hrg' +
    'NemXHOGY0gSiJxFPz%2FA3wJtW33IVJOoDhZi7zzrah4TaN4uQSVIGngUhxywsBinDLp' +
    'W8gTvVSdMwZn43haN%2Bov%2B8zfu4BPA79GD78ZdQUJ8%2Fn4ox15Vwp1E1EUO1tQvE' +
    'ujcukS5OkVRc7sUgBCaL3jDpRBN6LKvwfFt5sQEWPA%2FUBzOBx%2By4IFC0KLFy8mGo' +
    '0SCoUIBAKMjIywd%2B9e9uzZw%2FDwMCiAfwR4HmX2j6LB%2BCyqEduosLhaLIEtwF2o' +
    '5%2BxHGfdpVId1ofKlGbWCNwLT6uvrWbZsGXV1dYTD4axHIBBg9%2B7dtLa2gjxyDfAF' +
    'c92bEaH78S8ylAx%2Bs3AjmpJ%2FQEH9eeDbqDXrRBnxCMq6tmcOhUKh5YsWLQJgdHT0' +
    '5DE2NnbK36lUiubmZmKxGPF4vBFlZIDvApvQ4lICxcXXosG5FrV0s4EXKJ3w6opCPXA%' +
    '2B6nc%2FhnrkBPAbFPsGUL1nJSI7tWYj6WpNfX19ePbs2ad4WyQSyemJfX19bNy40d57' +
    'DfAT5JHLUYZeiVq%2BTPQjve9nqAyaMORL4BvQFHo3jmK7BRl4DCWNdnNktk7NaEVtaV' +
    '1dHQ0NDZ7EjSdw27ZtADtRsbwMhYMGc%2B2jwD%2BRYNCISqdFwLmoRAL16avNOTvITz' +
    'HKG14Engf8DmU8iwGUTTcgsjpQMO8me0D%2FIkowhEIhIpFIXuRFIhEOHz5MZ2cn6KFt' +
    '15RE03g9IjaJCt0IksHsMzWgyuAcNM2XoiJ%2FCyrwn0XkF7VE4EZgANVlvxxn%2BKOo' +
    'hOhFWfYo2b3Ooha1dVdkfuBGXDgcJh6P093dDSInjDTHx1GWH0Dhw6pFKTR4EaQWRc1r' +
    'e6TN%2B8uRanQxas%2B6UdeTzzp1VripMQHUNg2i%2BuuvqEBOoGl7BBHpNiWa0OifBp' +
    'swQJ5pyQsGgyQSCUZGTuaAl1BNuRNnda4fkdePCvMRY69deK8zttfiyPonkNc9a85bgh' +
    'LgH1FB7ke1cfXAEFo5C6Asm0SFcQciMJ8bXo1iFyC1cyZ6Iiv5dnpfI21sGEbZdwOSrm' +
    'Ie37Oibx3yNqtlNuLIcSnUSiaR%2BFEwvDywAZHVhZ61F7l9PoE4hBPsmY8C6hwUqBLm' +
    'ggdQRZwDMTQAAyiBvAb10nfgTWDa3CaBws0URGATIrEOJbi1qNhfjjqkguBWB4aQ0HkQ' +
    'PWO7MSTfyj%2BCSozrpqMs9CpE4AzzFFbos%2FMxAymk852P%2Bt91SIBdaU5fn6cdFm' +
    'NoGtulURsjpqG4ugL14AXBTQ8MINcfRIE87uPaXcDYAhRw5qLWpQUJgNOR5HtW9u8%2F' +
    'gLIlKIT0oDi4CRXNfkWENHqmDmNfL%2FLOA%2FhY5vUisAGl%2FhEKr59SKG71zELDPB' +
    'cRNg%2FJz43mmMppwXgfKpWS5ivdSO7qRwmlBXVCxWAUzapO5J07ya5%2BuyIfDxzAX8' +
    '85gozcnUJD3IyU1bk4kTyIRmhcXDiA2sI2NK3sIk87IvEQzsJVsbtVR3FifAM%2BMrEX' +
    'gSH87wxIG4MeO4TmzAk0bYPmgifMsc%2F5zmbg66jmG0IZ8kWUCOLoQXuQthgCfkDxgs' +
    'ggInEEH78AcJvzltw0%2FlWPHuCZvbBjMyxtRKyA5k0XitrHJVFZ%2Bd7WmOej%2Bmw1' +
    'jmf0mq9NRyRej6b0Op%2F2jbfTToqC4FXG2Crfr7LRjx74tgfh1gOw5FI0zAeBVti3Xz' +
    'uktmPiJZKpksD7jA32M1Cs6kLR4GGkdN%2BFem0%2Fm44sxtDAFSzQehG4CU3hQvamjM' +
    'coiluHga9uhdlb4YIA9KX1XifODqweFOd6UZ65wnzexqkzwE7l6aiGuxmtznnuY%2FHA' +
    'EI76nbdImw%2BBtozxi%2BMoh9j1kGNpZ6XL7hnsNscICh1LkKryhHlv%2FAxIm2vOQr' +
    'XgG9HaSbEEQm5BJCe8CLS7qYohMI1i2hCqoW2cGTLXzszydUiyAklR%2FVmuOYhIbAJu' +
    'QwvwtmYtBgUvD%2BRDYIdvcxykcLwshKOeZEMUEb0PKTnZtoakjV1NyGM3IVXl%2FhLY' +
    'WhC8ypgenMRZKrjtLbQK0EU4slmu%2B9u9OnG0PW5Vac3MD14EHiuXIQY1qF0%2BFy0T' +
    'eAkXveY4jmJpOX73cgrcCByl9N7nhSjypA0o02aLf%2BMxhKMJbqECP8h2IzBb7JloNA' +
    'LvRBJWjPwGsBeR2Immf1nhRmBJF1%2FyQA2S2ttRMT1%2BZc8NdkNTFxX4MXY5f%2Bbg' +
    'hXrgA2jLRwzv6WsxZs61G0HLislE4Cwk1jyHkkche%2FxilGkhPROThcBatOb8CCIvX%' +
    '2B%2BzsAtNZd8nM1kIjKL1jg2oOyl0o2QKxc6ye%2BFk%2BOV3AP3wcC%2FyPq%2FFol' +

    'yIUYH%2FEmUyeGAt2jz0JO6dhxeK0S19YzIQ2IBqTvtjmEnzY%2Bp8UGkCA0i6egFNwU' +
    'oU70Wh0jEwitTnEIWXLpMClfbAqSjwx%2FCfPCqKShIYQr3vIJL3ixFt%2Fy8RRsJp9b' +
    '%2B0q6KKKqqooooqfOB%2F6MmP5%2BlO7YkAAAAASUVORK5CYII%3D';
}
