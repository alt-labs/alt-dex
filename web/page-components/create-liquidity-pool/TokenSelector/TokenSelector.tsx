import { FC, useEffect, useState } from "react";
import useOnClickOutside from "../../../hooks/useOnClickOutside";
import { map } from "../../../util/ts-belt/Array";
import { not } from "../../../util/ts-belt/Boolean";
import { prop } from "../../../util/ts-belt/Dict";
import {
  SelectTokenButton,
  TokenAction,
  TokenDetails,
  TokenDropdown,
  TokenDropdownItem,
  TokenImage,
  TokenItemImage,
  TokenItemName,
  TokenItemSymbol,
  TokenSymbol,
  TokenWrapper,
} from "./styled";
import { SwapAsset } from "../../../lib/tx-builder/types";

type Props = {
  onTokenSelect: (token: SwapAsset) => void;
  selectedToken?: SwapAsset;
  disabledTokens: SwapAsset[];
  availableTokens: SwapAsset[];
};

const TokenSelector: FC<Props> = ({
  onTokenSelect,
  selectedToken,
  disabledTokens,
  availableTokens,
}) => {
  const [isTokenDropdownOpen, setIsTokenDropdownOpen] = useState(false);


  const dropdownRef = useOnClickOutside<HTMLUListElement>(() =>
    setIsTokenDropdownOpen(false)
  );

  const toggleDropdown = () => {
    setIsTokenDropdownOpen(not);
  };

  const handleTokenClick = (token: SwapAsset) => () => {
    onTokenSelect(token);
    setIsTokenDropdownOpen(false);
  };

  const disabledTokenSymbols: string[] = [
    selectedToken?.name || '',
    ...map(disabledTokens, prop("name")),
  ];

  return (
    <TokenWrapper>
      <SelectTokenButton onClick={toggleDropdown}>
        {selectedToken && (
          <>
            <TokenImage
              src={"https://assets.coingecko.com/coins/images/975/small/cardano.png?1547034860"}
              alt={selectedToken.name}
              width={44}
              height={44}
            />
            <TokenDetails>
              <TokenAction>Input</TokenAction>
              <TokenSymbol>{selectedToken.name}</TokenSymbol>
            </TokenDetails>
          </>
        )}
      </SelectTokenButton>
      <TokenDropdown isActive={isTokenDropdownOpen} ref={dropdownRef}>
        {availableTokens.map((token) => (
          <TokenDropdownItem
            key={token.name}
            isDisabled={disabledTokenSymbols.includes(token.name || "")}
            onClick={handleTokenClick(token)}
          >
            <TokenItemImage
              src={"https://assets.coingecko.com/coins/images/975/small/cardano.png?1547034860"}
              alt={token.name}
              width={24}
              height={24}
            />
            <div>
              <TokenItemSymbol>{token.name}</TokenItemSymbol>
              <TokenItemName>{token.name}</TokenItemName>
            </div>
          </TokenDropdownItem>
        ))}
      </TokenDropdown>
    </TokenWrapper>
  );
};

export default TokenSelector;
