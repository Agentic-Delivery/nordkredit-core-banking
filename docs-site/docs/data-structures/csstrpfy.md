---
id: DS-STRP-001
title: "Store PFKey Utility"
copybook_name: "CSSTRPFY.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (input handling)"
sidebar_position: 30
---

# DS-STRP-001: Store PFKey Utility (CSSTRPFY)

## Overview

The `CSSTRPFY.cpy` copybook defines the **Store PFKey Utility**, a procedure division code fragment that maps CICS terminal input AID (Attention Identifier) bytes to application-defined key identifiers. When a user presses a key on a 3270 terminal, CICS stores the key identity in the `EIBAID` field of the Execute Interface Block (EIB). This copybook translates those raw CICS AID values into the application's own key constants (defined in a separate structure).

This is a procedure division copybook, not a working storage data definition. It is included by every CICS program that needs to process keyboard input.

**Source file:** `CSSTRPFY.cpy`
**Used by:** ALL CICS programs

## Source COBOL

```cobol
EVALUATE EIBAID
    WHEN DFHENTER
        MOVE CCARD-AID-ENTER  TO CCARD-AID
    WHEN DFHCLEAR
        MOVE CCARD-AID-CLEAR  TO CCARD-AID
    WHEN DFHPA1
        MOVE CCARD-AID-PA1    TO CCARD-AID
    WHEN DFHPA2
        MOVE CCARD-AID-PA2    TO CCARD-AID
    WHEN DFHPF1
        MOVE CCARD-AID-PFK01  TO CCARD-AID
    WHEN DFHPF2
        MOVE CCARD-AID-PFK02  TO CCARD-AID
    WHEN DFHPF3
        MOVE CCARD-AID-PFK03  TO CCARD-AID
    WHEN DFHPF4
        MOVE CCARD-AID-PFK04  TO CCARD-AID
    WHEN DFHPF5
        MOVE CCARD-AID-PFK05  TO CCARD-AID
    WHEN DFHPF6
        MOVE CCARD-AID-PFK06  TO CCARD-AID
    WHEN DFHPF7
        MOVE CCARD-AID-PFK07  TO CCARD-AID
    WHEN DFHPF8
        MOVE CCARD-AID-PFK08  TO CCARD-AID
    WHEN DFHPF9
        MOVE CCARD-AID-PFK09  TO CCARD-AID
    WHEN DFHPF10
        MOVE CCARD-AID-PFK10  TO CCARD-AID
    WHEN DFHPF11
        MOVE CCARD-AID-PFK11  TO CCARD-AID
    WHEN DFHPF12
        MOVE CCARD-AID-PFK12  TO CCARD-AID
    WHEN DFHPF13
        MOVE CCARD-AID-PFK01  TO CCARD-AID
    WHEN DFHPF14
        MOVE CCARD-AID-PFK02  TO CCARD-AID
    WHEN DFHPF15
        MOVE CCARD-AID-PFK03  TO CCARD-AID
    WHEN DFHPF16
        MOVE CCARD-AID-PFK04  TO CCARD-AID
    WHEN DFHPF17
        MOVE CCARD-AID-PFK05  TO CCARD-AID
    WHEN DFHPF18
        MOVE CCARD-AID-PFK06  TO CCARD-AID
    WHEN DFHPF19
        MOVE CCARD-AID-PFK07  TO CCARD-AID
    WHEN DFHPF20
        MOVE CCARD-AID-PFK08  TO CCARD-AID
    WHEN DFHPF21
        MOVE CCARD-AID-PFK09  TO CCARD-AID
    WHEN DFHPF22
        MOVE CCARD-AID-PFK10  TO CCARD-AID
    WHEN DFHPF23
        MOVE CCARD-AID-PFK11  TO CCARD-AID
    WHEN DFHPF24
        MOVE CCARD-AID-PFK12  TO CCARD-AID
    WHEN OTHER
        MOVE CCARD-AID-ENTER  TO CCARD-AID
END-EVALUATE
```

## Key Mapping Table

| 3270 Key | CICS AID Constant | Application Constant | Common Function |
|----------|-------------------|---------------------|-----------------|
| Enter | `DFHENTER` | `CCARD-AID-ENTER` | Submit/confirm current screen |
| Clear | `DFHCLEAR` | `CCARD-AID-CLEAR` | Clear screen / reset |
| PA1 | `DFHPA1` | `CCARD-AID-PA1` | Attention (no data transmitted) |
| PA2 | `DFHPA2` | `CCARD-AID-PA2` | Attention (no data transmitted) |
| PF1 | `DFHPF1` | `CCARD-AID-PFK01` | Help |
| PF2 | `DFHPF2` | `CCARD-AID-PFK02` | (screen-specific) |
| PF3 | `DFHPF3` | `CCARD-AID-PFK03` | Back / Exit |
| PF4 | `DFHPF4` | `CCARD-AID-PFK04` | (screen-specific) |
| PF5 | `DFHPF5` | `CCARD-AID-PFK05` | (screen-specific) |
| PF6 | `DFHPF6` | `CCARD-AID-PFK06` | (screen-specific) |
| PF7 | `DFHPF7` | `CCARD-AID-PFK07` | Page Up / Scroll Back |
| PF8 | `DFHPF8` | `CCARD-AID-PFK08` | Page Down / Scroll Forward |
| PF9-PF12 | `DFHPF9`-`DFHPF12` | `CCARD-AID-PFK09`-`PFK12` | (screen-specific) |
| PF13-PF24 | `DFHPF13`-`DFHPF24` | `CCARD-AID-PFK01`-`PFK12` | **Mapped to PF1-PF12** (aliases) |

## Notes

1. **PF13-PF24 aliasing:** Keys PF13 through PF24 are mapped to the same application constants as PF1 through PF12. This is because some 3270 terminal models have 24 PF keys while others have only 12. By aliasing PF13-24 to PF1-12, the application works consistently on both terminal types.

2. **Default case:** The `WHEN OTHER` clause maps any unrecognized AID byte to `CCARD-AID-ENTER`. This means pressing any unsupported key is treated as pressing Enter. Individual programs then decide whether to accept or reject the action based on the mapped key value.

3. **Procedure division copybook:** Unlike most copybooks (which define data in WORKING-STORAGE), this copybook contains executable `EVALUATE` logic. It is included in the PROCEDURE DIVISION of each program, typically immediately after `EXEC CICS RECEIVE MAP`.

4. **CCARD-AID target variable:** The `CCARD-AID` variable (not defined in this copybook) is defined elsewhere in the application's working storage. It receives the translated key identifier for use by the program's input processing logic.

5. **Standard key conventions:** The CardDemo application follows common CICS conventions:
   - **Enter:** Submit the current form / confirm action
   - **PF3:** Go back / exit the current screen
   - **PF7/PF8:** Page up/down in list screens
   - **Clear:** Reset the screen or exit the application

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Input mechanism** | PF keys on 3270 terminal | HTML buttons, links, keyboard shortcuts |
| **Key identification** | `EIBAID` byte in EIB | HTTP method (GET/POST), form action, button `name`/`value` |
| **Navigation (PF3)** | `EVALUATE EIBAID WHEN DFHPF3` -> go back | Browser back button, Cancel link, breadcrumb navigation |
| **Form submission (Enter)** | `DFHENTER` -> process input | HTML form `POST` or button click |
| **Pagination (PF7/PF8)** | `DFHPF7`/`DFHPF8` -> page up/down | Pagination component with page numbers, previous/next links |
| **Help (PF1)** | `DFHPF1` -> display help | Help icon/link, tooltip, or contextual help panel |

### .NET Action Mapping (Conceptual)

```csharp
// The PF key -> action mapping is replaced by explicit UI controls
// Each PF key action becomes a distinct button or link in the web UI

// Example: List screen with pagination and navigation
public class TransactionListModel
{
    // PF7/PF8 (Page Up/Down) -> pagination parameters
    public int PageNumber { get; set; } = 1;
    public int PageSize { get; set; } = 20;

    // PF3 (Back) -> return URL
    public string ReturnUrl { get; set; }

    // Enter (Submit) -> form action
    [HttpPost]
    public IActionResult OnPost() { /* process form */ }
}
```

## Migration Notes

1. **No code to migrate:** This copybook contains procedure division code (an `EVALUATE` statement), not data definitions. The keyboard input handling pattern is entirely replaced by web UI interaction patterns (buttons, links, form submissions).

2. **Replace PF keys with explicit UI controls:** Each PF key function should become a visible, labeled UI control:
   - PF3 (Back) -> "Back" button or breadcrumb link
   - PF7/PF8 (Page) -> Pagination component
   - Enter (Submit) -> "Submit" / "Save" button
   - PF1 (Help) -> Help icon or link

3. **Keyboard shortcuts (optional):** For power users transitioning from 3270 terminals, the .NET UI could optionally support keyboard shortcuts (e.g., Escape for Back, Page Up/Down for list navigation) via JavaScript event handlers. This is a usability consideration, not a requirement.

4. **Eliminate aliasing:** The PF13-24 to PF1-12 aliasing was a hardware compatibility workaround. No equivalent is needed in the web UI.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
