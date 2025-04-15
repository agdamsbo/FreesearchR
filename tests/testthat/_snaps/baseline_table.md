# Creates correct table

    Code
      create_baseline(mtcars, by.var = "gear", add.p = "yes" == "yes", add.overall = TRUE,
      theme = "lancet")
    Message
      Setting theme "The Lancet"
    Output
      <div id="lyjxyyklkn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>#lyjxyyklkn table {
        font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }
      
      #lyjxyyklkn thead, #lyjxyyklkn tbody, #lyjxyyklkn tfoot, #lyjxyyklkn tr, #lyjxyyklkn td, #lyjxyyklkn th {
        border-style: none;
      }
      
      #lyjxyyklkn p {
        margin: 0;
        padding: 0;
      }
      
      #lyjxyyklkn .gt_table {
        display: table;
        border-collapse: collapse;
        line-height: normal;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #lyjxyyklkn .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #lyjxyyklkn .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 3px;
        padding-bottom: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #lyjxyyklkn .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #lyjxyyklkn .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #lyjxyyklkn .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #lyjxyyklkn .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #lyjxyyklkn .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #lyjxyyklkn .gt_spanner_row {
        border-bottom-style: hidden;
      }
      
      #lyjxyyklkn .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #lyjxyyklkn .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #lyjxyyklkn .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #lyjxyyklkn .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #lyjxyyklkn .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #lyjxyyklkn .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lyjxyyklkn .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #lyjxyyklkn .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #lyjxyyklkn .gt_row_group_first th {
        border-top-width: 2px;
      }
      
      #lyjxyyklkn .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lyjxyyklkn .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #lyjxyyklkn .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lyjxyyklkn .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_last_grand_summary_row_top {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: double;
        border-bottom-width: 6px;
        border-bottom-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #lyjxyyklkn .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lyjxyyklkn .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #lyjxyyklkn .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lyjxyyklkn .gt_left {
        text-align: left;
      }
      
      #lyjxyyklkn .gt_center {
        text-align: center;
      }
      
      #lyjxyyklkn .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #lyjxyyklkn .gt_font_normal {
        font-weight: normal;
      }
      
      #lyjxyyklkn .gt_font_bold {
        font-weight: bold;
      }
      
      #lyjxyyklkn .gt_font_italic {
        font-style: italic;
      }
      
      #lyjxyyklkn .gt_super {
        font-size: 65%;
      }
      
      #lyjxyyklkn .gt_footnote_marks {
        font-size: 75%;
        vertical-align: 0.4em;
        position: initial;
      }
      
      #lyjxyyklkn .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #lyjxyyklkn .gt_indent_1 {
        text-indent: 5px;
      }
      
      #lyjxyyklkn .gt_indent_2 {
        text-indent: 10px;
      }
      
      #lyjxyyklkn .gt_indent_3 {
        text-indent: 15px;
      }
      
      #lyjxyyklkn .gt_indent_4 {
        text-indent: 20px;
      }
      
      #lyjxyyklkn .gt_indent_5 {
        text-indent: 25px;
      }
      
      #lyjxyyklkn .katex-display {
        display: inline-flex !important;
        margin-bottom: 0.75em !important;
      }
      
      #lyjxyyklkn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
        height: 0px !important;
      }
      </style>
        <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
        <thead>
          <tr class="gt_col_headings">
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>Overall</strong><br />
      N = 32</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>3</strong><br />
      N = 15</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>4</strong><br />
      N = 12</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'><strong>5</strong><br />
      N = 5</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">mpg</td>
      <td headers="stat_0" class="gt_row gt_center">19·2 (15·4 – 22·8)</td>
      <td headers="stat_1" class="gt_row gt_center">15·5 (14·3 – 18·7)</td>
      <td headers="stat_2" class="gt_row gt_center">22·8 (21·0 – 28·9)</td>
      <td headers="stat_3" class="gt_row gt_center">19·7 (15·8 – 26·0)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0008</td></tr>
          <tr><td headers="label" class="gt_row gt_left">cyl</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td>
      <td headers="stat_1" class="gt_row gt_center"><br /></td>
      <td headers="stat_2" class="gt_row gt_center"><br /></td>
      <td headers="stat_3" class="gt_row gt_center"><br /></td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;"><0·0001</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">11 (34%)</td>
      <td headers="stat_1" class="gt_row gt_center">1 (6·7%)</td>
      <td headers="stat_2" class="gt_row gt_center">8 (67%)</td>
      <td headers="stat_3" class="gt_row gt_center">2 (40%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22%)</td>
      <td headers="stat_1" class="gt_row gt_center">2 (13%)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (33%)</td>
      <td headers="stat_3" class="gt_row gt_center">1 (20%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44%)</td>
      <td headers="stat_1" class="gt_row gt_center">12 (80%)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_3" class="gt_row gt_center">2 (40%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">disp</td>
      <td headers="stat_0" class="gt_row gt_center">196 (121 – 334)</td>
      <td headers="stat_1" class="gt_row gt_center">318 (276 – 400)</td>
      <td headers="stat_2" class="gt_row gt_center">131 (79 – 160)</td>
      <td headers="stat_3" class="gt_row gt_center">145 (120 – 301)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0003</td></tr>
          <tr><td headers="label" class="gt_row gt_left">hp</td>
      <td headers="stat_0" class="gt_row gt_center">123 (96 – 180)</td>
      <td headers="stat_1" class="gt_row gt_center">180 (150 – 215)</td>
      <td headers="stat_2" class="gt_row gt_center">94 (66 – 110)</td>
      <td headers="stat_3" class="gt_row gt_center">175 (113 – 264)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0007</td></tr>
          <tr><td headers="label" class="gt_row gt_left">drat</td>
      <td headers="stat_0" class="gt_row gt_center">3·70 (3·08 – 3·92)</td>
      <td headers="stat_1" class="gt_row gt_center">3·08 (3·00 – 3·21)</td>
      <td headers="stat_2" class="gt_row gt_center">3·92 (3·90 – 4·10)</td>
      <td headers="stat_3" class="gt_row gt_center">3·77 (3·62 – 4·22)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;"><0·0001</td></tr>
          <tr><td headers="label" class="gt_row gt_left">wt</td>
      <td headers="stat_0" class="gt_row gt_center">3·33 (2·54 – 3·65)</td>
      <td headers="stat_1" class="gt_row gt_center">3·73 (3·44 – 4·07)</td>
      <td headers="stat_2" class="gt_row gt_center">2·70 (2·07 – 3·17)</td>
      <td headers="stat_3" class="gt_row gt_center">2·77 (2·14 – 3·17)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0003</td></tr>
          <tr><td headers="label" class="gt_row gt_left">qsec</td>
      <td headers="stat_0" class="gt_row gt_center">17·71 (16·89 – 18·90)</td>
      <td headers="stat_1" class="gt_row gt_center">17·42 (17·02 – 18·00)</td>
      <td headers="stat_2" class="gt_row gt_center">18·76 (18·41 – 19·69)</td>
      <td headers="stat_3" class="gt_row gt_center">15·50 (14·60 – 16·70)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0017</td></tr>
          <tr><td headers="label" class="gt_row gt_left">vs</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44%)</td>
      <td headers="stat_1" class="gt_row gt_center">3 (20%)</td>
      <td headers="stat_2" class="gt_row gt_center">10 (83%)</td>
      <td headers="stat_3" class="gt_row gt_center">1 (20%)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;">0·0013</td></tr>
          <tr><td headers="label" class="gt_row gt_left">am</td>
      <td headers="stat_0" class="gt_row gt_center">13 (41%)</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_2" class="gt_row gt_center">8 (67%)</td>
      <td headers="stat_3" class="gt_row gt_center">5 (100%)</td>
      <td headers="p.value" class="gt_row gt_center" style="font-weight: bold;"><0·0001</td></tr>
          <tr><td headers="label" class="gt_row gt_left">carb</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td>
      <td headers="stat_1" class="gt_row gt_center"><br /></td>
      <td headers="stat_2" class="gt_row gt_center"><br /></td>
      <td headers="stat_3" class="gt_row gt_center"><br /></td>
      <td headers="p.value" class="gt_row gt_center">0·24</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    1</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22%)</td>
      <td headers="stat_1" class="gt_row gt_center">3 (20%)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (33%)</td>
      <td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    2</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31%)</td>
      <td headers="stat_1" class="gt_row gt_center">4 (27%)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (33%)</td>
      <td headers="stat_3" class="gt_row gt_center">2 (40%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_0" class="gt_row gt_center">3 (9·4%)</td>
      <td headers="stat_1" class="gt_row gt_center">3 (20%)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31%)</td>
      <td headers="stat_1" class="gt_row gt_center">5 (33%)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (33%)</td>
      <td headers="stat_3" class="gt_row gt_center">1 (20%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3·1%)</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_3" class="gt_row gt_center">1 (20%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3·1%)</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
      <td headers="stat_3" class="gt_row gt_center">1 (20%)</td>
      <td headers="p.value" class="gt_row gt_center"><br /></td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Median (IQR); n (%)</span></td>
          </tr>
          <tr>
            <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Kruskal-Wallis rank sum test; Fisher’s exact test</span></td>
          </tr>
        </tfoot>
      </table>
      </div>

---

    Code
      create_baseline(mtcars, by.var = "none", add.p = FALSE, add.overall = FALSE,
        theme = "lancet")
    Message
      Setting theme "The Lancet"
    Output
      <div id="lzlnuswsla" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>#lzlnuswsla table {
        font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }
      
      #lzlnuswsla thead, #lzlnuswsla tbody, #lzlnuswsla tfoot, #lzlnuswsla tr, #lzlnuswsla td, #lzlnuswsla th {
        border-style: none;
      }
      
      #lzlnuswsla p {
        margin: 0;
        padding: 0;
      }
      
      #lzlnuswsla .gt_table {
        display: table;
        border-collapse: collapse;
        line-height: normal;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #lzlnuswsla .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #lzlnuswsla .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 3px;
        padding-bottom: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #lzlnuswsla .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #lzlnuswsla .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #lzlnuswsla .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #lzlnuswsla .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #lzlnuswsla .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #lzlnuswsla .gt_spanner_row {
        border-bottom-style: hidden;
      }
      
      #lzlnuswsla .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #lzlnuswsla .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #lzlnuswsla .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #lzlnuswsla .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #lzlnuswsla .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #lzlnuswsla .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lzlnuswsla .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #lzlnuswsla .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #lzlnuswsla .gt_row_group_first th {
        border-top-width: 2px;
      }
      
      #lzlnuswsla .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lzlnuswsla .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #lzlnuswsla .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lzlnuswsla .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_last_grand_summary_row_top {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: double;
        border-bottom-width: 6px;
        border-bottom-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #lzlnuswsla .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lzlnuswsla .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #lzlnuswsla .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #lzlnuswsla .gt_left {
        text-align: left;
      }
      
      #lzlnuswsla .gt_center {
        text-align: center;
      }
      
      #lzlnuswsla .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #lzlnuswsla .gt_font_normal {
        font-weight: normal;
      }
      
      #lzlnuswsla .gt_font_bold {
        font-weight: bold;
      }
      
      #lzlnuswsla .gt_font_italic {
        font-style: italic;
      }
      
      #lzlnuswsla .gt_super {
        font-size: 65%;
      }
      
      #lzlnuswsla .gt_footnote_marks {
        font-size: 75%;
        vertical-align: 0.4em;
        position: initial;
      }
      
      #lzlnuswsla .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #lzlnuswsla .gt_indent_1 {
        text-indent: 5px;
      }
      
      #lzlnuswsla .gt_indent_2 {
        text-indent: 10px;
      }
      
      #lzlnuswsla .gt_indent_3 {
        text-indent: 15px;
      }
      
      #lzlnuswsla .gt_indent_4 {
        text-indent: 20px;
      }
      
      #lzlnuswsla .gt_indent_5 {
        text-indent: 25px;
      }
      
      #lzlnuswsla .katex-display {
        display: inline-flex !important;
        margin-bottom: 0.75em !important;
      }
      
      #lzlnuswsla div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
        height: 0px !important;
      }
      </style>
        <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
        <thead>
          <tr class="gt_col_headings">
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 32</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">mpg</td>
      <td headers="stat_0" class="gt_row gt_center">19·2 (15·4 – 22·8)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">cyl</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">11 (34%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">disp</td>
      <td headers="stat_0" class="gt_row gt_center">196 (121 – 334)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">hp</td>
      <td headers="stat_0" class="gt_row gt_center">123 (96 – 180)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">drat</td>
      <td headers="stat_0" class="gt_row gt_center">3·70 (3·08 – 3·92)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">wt</td>
      <td headers="stat_0" class="gt_row gt_center">3·33 (2·54 – 3·65)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">qsec</td>
      <td headers="stat_0" class="gt_row gt_center">17·71 (16·89 – 18·90)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">vs</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">am</td>
      <td headers="stat_0" class="gt_row gt_center">13 (41%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">gear</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_0" class="gt_row gt_center">15 (47%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">12 (38%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    5</td>
      <td headers="stat_0" class="gt_row gt_center">5 (16%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">carb</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    1</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    2</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_0" class="gt_row gt_center">3 (9·4%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3·1%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3·1%)</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Median (IQR); n (%)</span></td>
          </tr>
        </tfoot>
      </table>
      </div>

---

    Code
      create_baseline(mtcars, by.var = "test", add.p = FALSE, add.overall = FALSE,
        theme = "jama")
    Message
      Setting theme "JAMA"
    Output
      <div id="nnwsafkkdx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>#nnwsafkkdx table {
        font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }
      
      #nnwsafkkdx thead, #nnwsafkkdx tbody, #nnwsafkkdx tfoot, #nnwsafkkdx tr, #nnwsafkkdx td, #nnwsafkkdx th {
        border-style: none;
      }
      
      #nnwsafkkdx p {
        margin: 0;
        padding: 0;
      }
      
      #nnwsafkkdx .gt_table {
        display: table;
        border-collapse: collapse;
        line-height: normal;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #nnwsafkkdx .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #nnwsafkkdx .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 3px;
        padding-bottom: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #nnwsafkkdx .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #nnwsafkkdx .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #nnwsafkkdx .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #nnwsafkkdx .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #nnwsafkkdx .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #nnwsafkkdx .gt_spanner_row {
        border-bottom-style: hidden;
      }
      
      #nnwsafkkdx .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #nnwsafkkdx .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #nnwsafkkdx .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #nnwsafkkdx .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #nnwsafkkdx .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #nnwsafkkdx .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #nnwsafkkdx .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #nnwsafkkdx .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #nnwsafkkdx .gt_row_group_first th {
        border-top-width: 2px;
      }
      
      #nnwsafkkdx .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #nnwsafkkdx .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #nnwsafkkdx .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #nnwsafkkdx .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_last_grand_summary_row_top {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: double;
        border-bottom-width: 6px;
        border-bottom-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #nnwsafkkdx .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #nnwsafkkdx .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #nnwsafkkdx .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #nnwsafkkdx .gt_left {
        text-align: left;
      }
      
      #nnwsafkkdx .gt_center {
        text-align: center;
      }
      
      #nnwsafkkdx .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #nnwsafkkdx .gt_font_normal {
        font-weight: normal;
      }
      
      #nnwsafkkdx .gt_font_bold {
        font-weight: bold;
      }
      
      #nnwsafkkdx .gt_font_italic {
        font-style: italic;
      }
      
      #nnwsafkkdx .gt_super {
        font-size: 65%;
      }
      
      #nnwsafkkdx .gt_footnote_marks {
        font-size: 75%;
        vertical-align: 0.4em;
        position: initial;
      }
      
      #nnwsafkkdx .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #nnwsafkkdx .gt_indent_1 {
        text-indent: 5px;
      }
      
      #nnwsafkkdx .gt_indent_2 {
        text-indent: 10px;
      }
      
      #nnwsafkkdx .gt_indent_3 {
        text-indent: 15px;
      }
      
      #nnwsafkkdx .gt_indent_4 {
        text-indent: 20px;
      }
      
      #nnwsafkkdx .gt_indent_5 {
        text-indent: 25px;
      }
      
      #nnwsafkkdx .katex-display {
        display: inline-flex !important;
        margin-bottom: 0.75em !important;
      }
      
      #nnwsafkkdx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
        height: 0px !important;
      }
      </style>
        <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
        <thead>
          <tr class="gt_col_headings">
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 32</strong></span></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">mpg, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">19.2 (15.4 – 22.8)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">cyl, n (%)</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">11 (34)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">disp, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">196 (121 – 334)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">hp, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">123 (96 – 180)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">drat, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">3.70 (3.08 – 3.92)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">wt, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">3.33 (2.54 – 3.65)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">qsec, Median (IQR)</td>
      <td headers="stat_0" class="gt_row gt_center">17.71 (16.89 – 18.90)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">vs, n (%)</td>
      <td headers="stat_0" class="gt_row gt_center">14 (44)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">am, n (%)</td>
      <td headers="stat_0" class="gt_row gt_center">13 (41)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">gear, n (%)</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_0" class="gt_row gt_center">15 (47)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">12 (38)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    5</td>
      <td headers="stat_0" class="gt_row gt_center">5 (16)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">carb, n (%)</td>
      <td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    1</td>
      <td headers="stat_0" class="gt_row gt_center">7 (22)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    2</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_0" class="gt_row gt_center">3 (9.4)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_0" class="gt_row gt_center">10 (31)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3.1)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_0" class="gt_row gt_center">1 (3.1)</td></tr>
        </tbody>
        
        
      </table>
      </div>

---

    Code
      create_baseline(default_parsing(mtcars), by.var = "am", add.p = FALSE,
      add.overall = FALSE, theme = "nejm")
    Message
      Setting theme "New England Journal of Medicine"
    Output
      <div id="ybxdokjqel" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>#ybxdokjqel table {
        font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }
      
      #ybxdokjqel thead, #ybxdokjqel tbody, #ybxdokjqel tfoot, #ybxdokjqel tr, #ybxdokjqel td, #ybxdokjqel th {
        border-style: none;
      }
      
      #ybxdokjqel p {
        margin: 0;
        padding: 0;
      }
      
      #ybxdokjqel .gt_table {
        display: table;
        border-collapse: collapse;
        line-height: normal;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #ybxdokjqel .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #ybxdokjqel .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 3px;
        padding-bottom: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #ybxdokjqel .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #ybxdokjqel .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #ybxdokjqel .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #ybxdokjqel .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #ybxdokjqel .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #ybxdokjqel .gt_spanner_row {
        border-bottom-style: hidden;
      }
      
      #ybxdokjqel .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #ybxdokjqel .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #ybxdokjqel .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #ybxdokjqel .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #ybxdokjqel .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #ybxdokjqel .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ybxdokjqel .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #ybxdokjqel .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #ybxdokjqel .gt_row_group_first th {
        border-top-width: 2px;
      }
      
      #ybxdokjqel .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ybxdokjqel .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #ybxdokjqel .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ybxdokjqel .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_last_grand_summary_row_top {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: double;
        border-bottom-width: 6px;
        border-bottom-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #ybxdokjqel .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ybxdokjqel .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #ybxdokjqel .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ybxdokjqel .gt_left {
        text-align: left;
      }
      
      #ybxdokjqel .gt_center {
        text-align: center;
      }
      
      #ybxdokjqel .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #ybxdokjqel .gt_font_normal {
        font-weight: normal;
      }
      
      #ybxdokjqel .gt_font_bold {
        font-weight: bold;
      }
      
      #ybxdokjqel .gt_font_italic {
        font-style: italic;
      }
      
      #ybxdokjqel .gt_super {
        font-size: 65%;
      }
      
      #ybxdokjqel .gt_footnote_marks {
        font-size: 75%;
        vertical-align: 0.4em;
        position: initial;
      }
      
      #ybxdokjqel .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #ybxdokjqel .gt_indent_1 {
        text-indent: 5px;
      }
      
      #ybxdokjqel .gt_indent_2 {
        text-indent: 10px;
      }
      
      #ybxdokjqel .gt_indent_3 {
        text-indent: 15px;
      }
      
      #ybxdokjqel .gt_indent_4 {
        text-indent: 20px;
      }
      
      #ybxdokjqel .gt_indent_5 {
        text-indent: 25px;
      }
      
      #ybxdokjqel .katex-display {
        display: inline-flex !important;
        margin-bottom: 0.75em !important;
      }
      
      #ybxdokjqel div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
        height: 0px !important;
      }
      </style>
        <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
        <thead>
          <tr class="gt_col_headings">
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>FALSE</strong><br />
      N = 19</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>TRUE</strong><br />
      N = 13</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">mpg</td>
      <td headers="stat_1" class="gt_row gt_center">17.3 (14.7 – 19.2)</td>
      <td headers="stat_2" class="gt_row gt_center">22.8 (21.0 – 30.4)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">cyl</td>
      <td headers="stat_1" class="gt_row gt_center"><br /></td>
      <td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_1" class="gt_row gt_center">3 (16)</td>
      <td headers="stat_2" class="gt_row gt_center">8 (62)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_1" class="gt_row gt_center">4 (21)</td>
      <td headers="stat_2" class="gt_row gt_center">3 (23)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_1" class="gt_row gt_center">12 (63)</td>
      <td headers="stat_2" class="gt_row gt_center">2 (15)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">disp</td>
      <td headers="stat_1" class="gt_row gt_center">276 (168 – 360)</td>
      <td headers="stat_2" class="gt_row gt_center">120 (79 – 160)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">hp</td>
      <td headers="stat_1" class="gt_row gt_center">175 (110 – 205)</td>
      <td headers="stat_2" class="gt_row gt_center">109 (66 – 113)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">drat</td>
      <td headers="stat_1" class="gt_row gt_center">3.15 (3.07 – 3.70)</td>
      <td headers="stat_2" class="gt_row gt_center">4.08 (3.85 – 4.22)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">wt</td>
      <td headers="stat_1" class="gt_row gt_center">3.52 (3.44 – 3.85)</td>
      <td headers="stat_2" class="gt_row gt_center">2.32 (1.94 – 2.78)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">qsec</td>
      <td headers="stat_1" class="gt_row gt_center">17.82 (17.05 – 19.44)</td>
      <td headers="stat_2" class="gt_row gt_center">17.02 (16.46 – 18.61)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">vs</td>
      <td headers="stat_1" class="gt_row gt_center">7 (37)</td>
      <td headers="stat_2" class="gt_row gt_center">7 (54)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">gear</td>
      <td headers="stat_1" class="gt_row gt_center"><br /></td>
      <td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_1" class="gt_row gt_center">15 (79)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_1" class="gt_row gt_center">4 (21)</td>
      <td headers="stat_2" class="gt_row gt_center">8 (62)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    5</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0)</td>
      <td headers="stat_2" class="gt_row gt_center">5 (38)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">carb</td>
      <td headers="stat_1" class="gt_row gt_center"><br /></td>
      <td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    1</td>
      <td headers="stat_1" class="gt_row gt_center">3 (16)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (31)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    2</td>
      <td headers="stat_1" class="gt_row gt_center">6 (32)</td>
      <td headers="stat_2" class="gt_row gt_center">4 (31)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    3</td>
      <td headers="stat_1" class="gt_row gt_center">3 (16)</td>
      <td headers="stat_2" class="gt_row gt_center">0 (0)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    4</td>
      <td headers="stat_1" class="gt_row gt_center">7 (37)</td>
      <td headers="stat_2" class="gt_row gt_center">3 (23)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    6</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0)</td>
      <td headers="stat_2" class="gt_row gt_center">1 (7.7)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    8</td>
      <td headers="stat_1" class="gt_row gt_center">0 (0)</td>
      <td headers="stat_2" class="gt_row gt_center">1 (7.7)</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Median (IQR); n (%)</span></td>
          </tr>
        </tfoot>
      </table>
      </div>

