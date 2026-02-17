namespace NordKredit.Domain.Payments.Messaging;

/// <summary>
/// Payment clearing message for Bankgirot (Swedish domestic payments).
/// Maps to ISO 20022 pain.001 (CustomerCreditTransferInitiation) format.
///
/// COBOL source: Payment programs using IBM MQ for Bankgirot clearing.
/// Regulations: FFFS 2014:5 Ch. 8 (accurate records), PSD2 Art. 97 (SCA).
/// Bankgirot: Swedish payment clearing system operated by Bankgirocentralen BGC AB.
/// </summary>
public sealed record BankgirotPaymentMessage
{
    /// <summary>
    /// Unique message identification (ISO 20022: MsgId).
    /// </summary>
    public required string MessageId { get; init; }

    /// <summary>
    /// Creation date and time of the message (ISO 20022: CreDtTm).
    /// </summary>
    public required DateTimeOffset CreationDateTime { get; init; }

    /// <summary>
    /// Number of individual transactions in the message (ISO 20022: NbOfTxs).
    /// </summary>
    public required int NumberOfTransactions { get; init; }

    /// <summary>
    /// Total amount of all transactions in the message (ISO 20022: CtrlSum).
    /// COBOL: PIC S9(13)V99 — decimal(15,2).
    /// </summary>
    public required decimal ControlSum { get; init; }

    /// <summary>
    /// Debtor name — the paying party (ISO 20022: Dbtr/Nm).
    /// </summary>
    public required string DebtorName { get; init; }

    /// <summary>
    /// Debtor account IBAN (ISO 20022: DbtrAcct/Id/IBAN).
    /// Swedish IBANs: SE + 2 check digits + 20 digits.
    /// </summary>
    public required string DebtorIban { get; init; }

    /// <summary>
    /// Debtor agent BIC (ISO 20022: DbtrAgt/FinInstnId/BIC).
    /// NordKredit BIC identifier.
    /// </summary>
    public required string DebtorAgentBic { get; init; }

    /// <summary>
    /// Creditor name — the receiving party (ISO 20022: Cdtr/Nm).
    /// </summary>
    public required string CreditorName { get; init; }

    /// <summary>
    /// Creditor Bankgiro number (ISO 20022: CdtrAcct/Id/Othr/Id).
    /// Bankgiro numbers: 7-8 digits.
    /// </summary>
    public required string CreditorBankgiroNumber { get; init; }

    /// <summary>
    /// Creditor agent BIC (ISO 20022: CdtrAgt/FinInstnId/BIC).
    /// Receiving bank's BIC identifier.
    /// </summary>
    public required string CreditorAgentBic { get; init; }

    /// <summary>
    /// Payment amount in SEK (ISO 20022: InstdAmt).
    /// COBOL: PIC S9(13)V99 — decimal(15,2).
    /// </summary>
    public required decimal Amount { get; init; }

    /// <summary>
    /// Currency code — always SEK for Bankgirot (ISO 20022: InstdAmt/@Ccy).
    /// </summary>
    public required string Currency { get; init; }

    /// <summary>
    /// Requested execution date (ISO 20022: ReqdExctnDt).
    /// </summary>
    public required DateOnly RequestedExecutionDate { get; init; }

    /// <summary>
    /// Payment reference — OCR number or free text (ISO 20022: RmtInf/Strd/CdtrRefInf/Ref).
    /// Swedish OCR number used for automatic reconciliation.
    /// </summary>
    public string? PaymentReference { get; init; }

    /// <summary>
    /// End-to-end identification for tracing (ISO 20022: EndToEndId).
    /// </summary>
    public required string EndToEndId { get; init; }
}
