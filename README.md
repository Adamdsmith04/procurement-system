

# Public Procurement Transparency System

A decentralized application for transparent government procurement processes built on Stacks blockchain using Clarity smart contracts.

## Overview

This smart contract implements a transparent public procurement system that allows:

- Government entities to create tender notices
- Businesses to submit bids for contracts
- Transparent evaluation and award processes
- Immutable record-keeping of all procurement activities

## Key Features

- **Open Bidding**: Any qualified business can submit bids for government contracts
- **Immutable Records**: All bids and contract details are permanently stored on the blockchain
- **Transparent Selection**: Winner selection based on predefined criteria
- **Qualification System**: Businesses are evaluated based on experience, previous contracts, certification, and financial stability

## Contract Functions

### For Government Entities (Contract Owner)

- `create-tender`: Create a new tender with details and requirements
- `register-bidder-qualification`: Register or update a bidder's qualification metrics
- `close-tender`: Close a tender for new bids
- `evaluate-bid`: Evaluate a submitted bid with scores for technical, financial, and compliance aspects
- `award-tender`: Award a tender to the winning bid
- `finalize-tender`: Mark a tender as completed
- `cancel-tender`: Cancel a tender with a reason

### For Businesses (Bidders)

- `submit-bid`: Submit a bid for an open tender

### Read-Only Functions

- `get-tender`: Get details of a specific tender
- `get-bid`: Get details of a specific bid
- `get-tender-bids`: Get all bids for a specific tender
- `get-bidder-qualification`: Get qualification details for a bidder
- `get-tender-evaluation`: Get evaluation details for a bid
- `calculate-qualification-score`: Calculate a bidder's qualification score
- `get-bid-total-score`: Get the total evaluation score for a bid
- `get-winning-bid`: Get the winning bid for a tender
- `get-all-tender-bids`: Get all bids for a tender

## Usage Example

1. Government creates a tender:
```clarity
(contract-call? .procurement-system create-tender "Highway Construction Project" "Construction of 5km highway section" u1000000 u10000 u50 "0x1234567890abcdef")
```

2. Register bidder qualifications:
```clarity
(contract-call? .procurement-system register-bidder-qualification 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u5 u10 u3 u80)
```

3. Bidder submits a bid:
```clarity
(contract-call? .procurement-system submit-bid u1 u950000 "0xabcdef1234567890")
```

4. Government closes the tender:
```clarity
(contract-call? .procurement-system close-tender u1)
```

5. Government evaluates bids:
```clarity
(contract-call? .procurement-system evaluate-bid u1 u1 u85 u90 u95 "Excellent technical proposal with reasonable cost")
```

6. Government awards the tender:
```clarity
(contract-call? .procurement-system award-tender u1 u1)
```

7. Government finalizes the tender after completion:
```clarity
(contract-call? .procurement-system finalize-tender u1)
```

## Error Codes

- `u100`: Not the contract owner
- `u101`: Item not found
- `u102`: Tender is closed
- `u103`: Tender is still open
- `u104`: Invalid bid
- `u105`: Already exists
- `u106`: Not eligible
- `u107`: Already awarded
- `u108`: Not awarded
```
