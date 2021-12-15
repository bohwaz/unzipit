/* unzipit@1.3.6, license MIT */
/* This file doesn't have support for: inflate (only uncompressed files are supported), HTTP, ArrayBuffer, workers
/* global SharedArrayBuffer, process */
(function () {
	function readBlobAsArrayBuffer(blob) {
		if (blob.arrayBuffer) {
			return blob.arrayBuffer();
		}
		return new Promise((resolve, reject) => {
			const reader = new FileReader();
			reader.addEventListener('loadend', () => {
				resolve(reader.result);
			});
			reader.addEventListener('error', reject);
			reader.readAsArrayBuffer(blob);
		});
	}

	async function readBlobAsUint8Array(blob) {
		const arrayBuffer = await readBlobAsArrayBuffer(blob);
		return new Uint8Array(arrayBuffer);
	}

	function isBlob(v) {
		return typeof Blob !== 'undefined' && v instanceof Blob;
	}

	function isSharedArrayBuffer(b) {
		return typeof SharedArrayBuffer !== 'undefined' && b instanceof SharedArrayBuffer;
	}

	const isNode =
			(typeof process !== 'undefined') &&
			process.versions &&
			(typeof process.versions.node !== 'undefined') &&
			(typeof process.versions.electron === 'undefined');

	function isTypedArraySameAsArrayBuffer(typedArray) {
		return typedArray.byteOffset === 0 && typedArray.byteLength === typedArray.buffer.byteLength;
	}


	class BlobReader {
		constructor(blob) {
			this.blob = blob;
		}
		async getLength() {
			return this.blob.size;
		}
		async read(offset, length) {
			const blob = this.blob.slice(offset, offset + length);
			const arrayBuffer = await readBlobAsArrayBuffer(blob);
			return new Uint8Array(arrayBuffer);
		}
		async sliceAsBlob(offset, length, type = '') {
			return this.blob.slice(offset, offset + length, type);
		}
	}

	function makeCodes(tree, MAX_BITS) {  // code, length
		var max_code = tree.length;
		var code, bits, n, i, len;

		var bl_count = U.bl_count;  for(var i=0; i<=MAX_BITS; i++) bl_count[i]=0;
		for(i=1; i<max_code; i+=2) bl_count[tree[i]]++;

		var next_code = U.next_code;	// smallest code for each length

		code = 0;
		bl_count[0] = 0;
		for (bits = 1; bits <= MAX_BITS; bits++) {
			code = (code + bl_count[bits-1]) << 1;
			next_code[bits] = code;
		}

		for (n = 0; n < max_code; n+=2) {
			len = tree[n+1];
			if (len != 0) {
				tree[n] = next_code[len];
				next_code[len]++;
			}
		}
	}
	function codes2map(tree, MAX_BITS, map) {
		var max_code = tree.length;
		var r15 = U.rev15;
		for(var i=0; i<max_code; i+=2) if(tree[i+1]!=0)  {
			var lit = i>>1;
			var cl = tree[i+1], val = (lit<<4)|cl; // :  (0x8000 | (U.of0[lit-257]<<7) | (U.exb[lit-257]<<4) | cl);
			var rest = (MAX_BITS-cl), i0 = tree[i]<<rest, i1 = i0 + (1<<rest);
			//tree[i]=r15[i0]>>>(15-MAX_BITS);
			while(i0!=i1) {
				var p0 = r15[i0]>>>(15-MAX_BITS);
				map[p0]=val;  i0++;
			}
		}
	}
	function revCodes(tree, MAX_BITS) {
		var r15 = U.rev15, imb = 15-MAX_BITS;
		for(var i=0; i<tree.length; i+=2) {  var i0 = (tree[i]<<(MAX_BITS-tree[i+1]));  tree[i] = r15[i0]>>>imb;  }
	}

	function _bitsE(dt, pos, length) {  return ((dt[pos>>>3] | (dt[(pos>>>3)+1]<<8)                        )>>>(pos&7))&((1<<length)-1);  }
	function _bitsF(dt, pos, length) {  return ((dt[pos>>>3] | (dt[(pos>>>3)+1]<<8) | (dt[(pos>>>3)+2]<<16))>>>(pos&7))&((1<<length)-1);  }
	/*
	function _get9(dt, pos) {
		return ((dt[pos>>>3] | (dt[(pos>>>3)+1]<<8))>>>(pos&7))&511;
	} */
	function _get17(dt, pos) {	// return at least 17 meaningful bytes
		return (dt[pos>>>3] | (dt[(pos>>>3)+1]<<8) | (dt[(pos>>>3)+2]<<16) )>>>(pos&7);
	}
	const U = function(){
		var u16=Uint16Array, u32=Uint32Array;
		return {
			next_code : new u16(16),
			bl_count  : new u16(16),
			ordr : [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ],
			of0  : [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,999,999,999],
			exb  : [0,0,0,0,0,0,0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4,  4,  5,  5,  5,  5,  0,  0,  0,  0],
			ldef : new u16(32),
			df0  : [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577, 65535, 65535],
			dxb  : [0,0,0,0,1,1,2, 2, 3, 3, 4, 4, 5, 5,  6,  6,  7,  7,  8,  8,   9,   9,  10,  10,  11,  11,  12,   12,   13,   13,     0,     0],
			ddef : new u32(32),
			flmap: new u16(  512),  fltree: [],
			fdmap: new u16(   32),  fdtree: [],
			lmap : new u16(32768),  ltree : [],  ttree:[],
			dmap : new u16(32768),  dtree : [],
			imap : new u16(  512),  itree : [],
			//rev9 : new u16(  512)
			rev15: new u16(1<<15),
			lhst : new u32(286), dhst : new u32( 30), ihst : new u32(19),
			lits : new u32(15000),
			strt : new u16(1<<16),
			prev : new u16(1<<15)
		};
	} ();

	(function(){
		var len = 1<<15;
		for(var i=0; i<len; i++) {
			var x = i;
			x = (((x & 0xaaaaaaaa) >>> 1) | ((x & 0x55555555) << 1));
			x = (((x & 0xcccccccc) >>> 2) | ((x & 0x33333333) << 2));
			x = (((x & 0xf0f0f0f0) >>> 4) | ((x & 0x0f0f0f0f) << 4));
			x = (((x & 0xff00ff00) >>> 8) | ((x & 0x00ff00ff) << 8));
			U.rev15[i] = (((x >>> 16) | (x << 16)))>>>17;
		}

		function pushV(tgt, n, sv) {  while(n--!=0) tgt.push(0,sv);  }

		for(var i=0; i<32; i++) {  U.ldef[i]=(U.of0[i]<<3)|U.exb[i];  U.ddef[i]=(U.df0[i]<<4)|U.dxb[i];  }

		pushV(U.fltree, 144, 8);  pushV(U.fltree, 255-143, 9);  pushV(U.fltree, 279-255, 7);  pushV(U.fltree,287-279,8);
		/*
		var i = 0;
		for(; i<=143; i++) U.fltree.push(0,8);
		for(; i<=255; i++) U.fltree.push(0,9);
		for(; i<=279; i++) U.fltree.push(0,7);
		for(; i<=287; i++) U.fltree.push(0,8);
		*/
		makeCodes(U.fltree, 9);
		codes2map(U.fltree, 9, U.flmap);
		revCodes (U.fltree, 9);

		pushV(U.fdtree,32,5);
		//for(i=0;i<32; i++) U.fdtree.push(0,5);
		makeCodes(U.fdtree, 5);
		codes2map(U.fdtree, 5, U.fdmap);
		revCodes (U.fdtree, 5);

		pushV(U.itree,19,0);  pushV(U.ltree,286,0);  pushV(U.dtree,30,0);  pushV(U.ttree,320,0);
		/*
		for(var i=0; i< 19; i++) U.itree.push(0,0);
		for(var i=0; i<286; i++) U.ltree.push(0,0);
		for(var i=0; i< 30; i++) U.dtree.push(0,0);
		for(var i=0; i<320; i++) U.ttree.push(0,0);
		*/
	})();

	const crc = {
		table : ( function() {
			 var tab = new Uint32Array(256);
			 for (var n=0; n<256; n++) {
				var c = n;
				for (var k=0; k<8; k++) {
					if (c & 1)  c = 0xedb88320 ^ (c >>> 1);
					else        c = c >>> 1;
				}
				tab[n] = c;  }
			return tab;  })(),
		update : function(c, buf, off, len) {
			for (var i=0; i<len; i++)  c = crc.table[(c ^ buf[off+i]) & 0xff] ^ (c >>> 8);
			return c;
		},
		crc : function(b,o,l)  {  return crc.update(0xffffffff,b,o,l) ^ 0xffffffff;  }
	};


	/*
	class Zip {
		constructor(reader) {
			comment,  // the comment for this entry
			commentBytes, // the raw comment for this entry
		}
	}
	*/

	class ZipEntry {
		constructor(reader, rawEntry) {
			this._reader = reader;
			this._rawEntry = rawEntry;
			this.name = rawEntry.name;
			this.nameBytes = rawEntry.nameBytes;
			this.size = rawEntry.uncompressedSize;
			this.compressedSize = rawEntry.compressedSize;
			this.comment = rawEntry.comment;
			this.commentBytes = rawEntry.commentBytes;
			this.compressionMethod = rawEntry.compressionMethod;
			this.lastModDate = null;
			this.isDirectory = rawEntry.uncompressedSize === 0 && rawEntry.name.endsWith('/');
			this.encrypted = !!(rawEntry.generalPurposeBitFlag & 0x1);
		}
		// returns a promise that returns a Blob for this entry
		async blob(type = 'application/octet-stream') {
			return await readEntryDataAsBlob(this._reader, this._rawEntry, type);
		}
		// returns a promise that returns an ArrayBuffer for this entry
		async arrayBuffer() {
			return await readEntryDataAsArrayBuffer(this._reader, this._rawEntry);
		}
		// returns text, assumes the text is valid utf8. If you want more options decode arrayBuffer yourself
		async text() {
			const buffer = await this.arrayBuffer();
			return decodeBuffer(new Uint8Array(buffer));
		}
	}

	const EOCDR_WITHOUT_COMMENT_SIZE = 22;
	const MAX_COMMENT_SIZE = 0xffff; // 2-byte size
	const EOCDR_SIGNATURE = 0x06054b50;
	const ZIP64_EOCDR_SIGNATURE = 0x06064b50;

	async function readAs(reader, offset, length) {
		return await reader.read(offset, length);
	}

	// The point of this function is we want to be able to pass the data
	// to a worker as fast as possible so when decompressing if the data
	// is already a blob and we can get a blob then get a blob.
	//
	// I'm not sure what a better way to refactor this is. We've got examples
	// of multiple readers. Ideally, for every type of reader we could ask
	// it, "give me a type that is zero copy both locally and when sent to a worker".
	//
	// The problem is the worker would also have to know the how to handle this
	// opaque type. I suppose the correct solution is to register different
	// reader handlers in the worker so BlobReader would register some
	// `handleZeroCopyType<BlobReader>`. At the moment I don't feel like
	// refactoring. As it is you just pass in an instance of the reader
	// but instead you'd have to register the reader and some how get the
	// source for the `handleZeroCopyType` handler function into the worker.
	// That sounds like a huge PITA, requiring you to put the implementation
	// in a separate file so the worker can load it or some other workaround
	// hack.
	//
	// For now this hack works even if it's not generic.
	async function readAsBlobOrTypedArray(reader, offset, length, type) {
		if (reader.sliceAsBlob) {
			return await reader.sliceAsBlob(offset, length, type);
		}
		return await reader.read(offset, length);
	}

	const crc$1 = {
		unsigned() {
			return 0;
		},
	};

	function getUint16LE(uint8View, offset) {
		return uint8View[offset    ] +
					 uint8View[offset + 1] * 0x100;
	}

	function getUint32LE(uint8View, offset) {
		return uint8View[offset    ] +
					 uint8View[offset + 1] * 0x100 +
					 uint8View[offset + 2] * 0x10000 +
					 uint8View[offset + 3] * 0x1000000;
	}

	function getUint64LE(uint8View, offset) {
		return getUint32LE(uint8View, offset) +
					 getUint32LE(uint8View, offset + 4) * 0x100000000;
	}

	/* eslint-disable no-irregular-whitespace */
	// const decodeCP437 = (function() {
	//   const cp437 = '\u0000☺☻♥♦♣♠•◘○◙♂♀♪♫☼►◄↕‼¶§▬↨↑↓→←∟↔▲▼ !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~⌂ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀αßΓπΣσµτΦΘΩδ∞φε∩≡±≥≤⌠⌡÷≈°∙·√ⁿ²■ ';
	//
	//   return function(uint8view) {
	//     return Array.from(uint8view).map(v => cp437[v]).join('');
	//   };
	// }());
	/* eslint-enable no-irregular-whitespace */

	const utf8Decoder = new TextDecoder();
	function decodeBuffer(uint8View, isUTF8) {  /* eslint-disable-line no-unused-vars */ /* lgtm [js/superfluous-trailing-arguments] */
		if (isSharedArrayBuffer(uint8View.buffer)) {
			uint8View = new Uint8Array(uint8View);
		}
		return utf8Decoder.decode(uint8View);
		/*
		AFAICT the UTF8 flat is not set so it's 100% up to the user
		to self decode if their file is not utf8 filenames
		return isUTF8
				? utf8Decoder.decode(uint8View)
				: decodeCP437(uint8View);
		*/
	}

	async function findEndOfCentralDirector(reader, totalLength) {
		const size = Math.min(EOCDR_WITHOUT_COMMENT_SIZE + MAX_COMMENT_SIZE, totalLength);
		const readStart = totalLength - size;
		const data = await readAs(reader, readStart, size);
		for (let i = size - EOCDR_WITHOUT_COMMENT_SIZE; i >= 0; --i) {
			if (getUint32LE(data, i) !== EOCDR_SIGNATURE) {
				continue;
			}

			// 0 - End of central directory signature
			const eocdr = new Uint8Array(data.buffer, data.byteOffset + i, data.byteLength - i);
			// 4 - Number of this disk
			const diskNumber = getUint16LE(eocdr, 4);
			if (diskNumber !== 0) {
				throw new Error(`multi-volume zip files are not supported. This is volume: ${diskNumber}`);
			}

			// 6 - Disk where central directory starts
			// 8 - Number of central directory records on this disk
			// 10 - Total number of central directory records
			const entryCount = getUint16LE(eocdr, 10);
			// 12 - Size of central directory (bytes)
			const centralDirectorySize = getUint32LE(eocdr, 12);
			// 16 - Offset of start of central directory, relative to start of archive
			const centralDirectoryOffset = getUint32LE(eocdr, 16);
			// 20 - Comment length
			const commentLength = getUint16LE(eocdr, 20);
			const expectedCommentLength = eocdr.length - EOCDR_WITHOUT_COMMENT_SIZE;
			if (commentLength !== expectedCommentLength) {
				throw new Error(`invalid comment length. expected: ${expectedCommentLength}, actual: ${commentLength}`);
			}

			// 22 - Comment
			// the encoding is always cp437.
			const commentBytes = new Uint8Array(eocdr.buffer, eocdr.byteOffset + 22, commentLength);
			const comment = decodeBuffer(commentBytes);

			if (entryCount === 0xffff || centralDirectoryOffset === 0xffffffff) {
				return await readZip64CentralDirectory(reader, readStart + i, comment, commentBytes);
			} else {
				return await readEntries(reader, centralDirectoryOffset, centralDirectorySize, entryCount, comment, commentBytes);
			}
		}

		throw new Error('could not find end of central directory. maybe not zip file');
	}

	const END_OF_CENTRAL_DIRECTORY_LOCATOR_SIGNATURE = 0x07064b50;

	async function readZip64CentralDirectory(reader, offset, comment, commentBytes) {
		// ZIP64 Zip64 end of central directory locator
		const zip64EocdlOffset = offset - 20;
		const eocdl = await readAs(reader, zip64EocdlOffset, 20);

		// 0 - zip64 end of central dir locator signature
		if (getUint32LE(eocdl, 0) !== END_OF_CENTRAL_DIRECTORY_LOCATOR_SIGNATURE) {
			throw new Error('invalid zip64 end of central directory locator signature');
		}

		// 4 - number of the disk with the start of the zip64 end of central directory
		// 8 - relative offset of the zip64 end of central directory record
		const zip64EocdrOffset = getUint64LE(eocdl, 8);
		// 16 - total number of disks

		// ZIP64 end of central directory record
		const zip64Eocdr = await readAs(reader, zip64EocdrOffset, 56);

		// 0 - zip64 end of central dir signature                           4 bytes  (0x06064b50)
		if (getUint32LE(zip64Eocdr, 0) !== ZIP64_EOCDR_SIGNATURE) {
			throw new Error('invalid zip64 end of central directory record signature');
		}
		// 4 - size of zip64 end of central directory record                8 bytes
		// 12 - version made by                                             2 bytes
		// 14 - version needed to extract                                   2 bytes
		// 16 - number of this disk                                         4 bytes
		// 20 - number of the disk with the start of the central directory  4 bytes
		// 24 - total number of entries in the central directory on this disk         8 bytes
		// 32 - total number of entries in the central directory            8 bytes
		const entryCount = getUint64LE(zip64Eocdr, 32);
		// 40 - size of the central directory                               8 bytes
		const centralDirectorySize = getUint64LE(zip64Eocdr, 40);
		// 48 - offset of start of central directory with respect to the starting disk number     8 bytes
		const centralDirectoryOffset = getUint64LE(zip64Eocdr, 48);
		// 56 - zip64 extensible data sector                                (variable size)
		return readEntries(reader, centralDirectoryOffset, centralDirectorySize, entryCount, comment, commentBytes);
	}

	const CENTRAL_DIRECTORY_FILE_HEADER_SIGNATURE = 0x02014b50;

	async function readEntries(reader, centralDirectoryOffset, centralDirectorySize, rawEntryCount, comment, commentBytes) {
		let readEntryCursor = 0;
		const allEntriesBuffer = await readAs(reader, centralDirectoryOffset, centralDirectorySize);
		const rawEntries = [];

		for (let e = 0; e < rawEntryCount; ++e) {
			const buffer = allEntriesBuffer.subarray(readEntryCursor, readEntryCursor + 46);
			// 0 - Central directory file header signature
			const signature = getUint32LE(buffer, 0);
			if (signature !== CENTRAL_DIRECTORY_FILE_HEADER_SIGNATURE) {
				throw new Error(`invalid central directory file header signature: 0x${signature.toString(16)}`);
			}
			const rawEntry = {
				// 4 - Version made by
				versionMadeBy: getUint16LE(buffer, 4),
				// 6 - Version needed to extract (minimum)
				versionNeededToExtract: getUint16LE(buffer, 6),
				// 8 - General purpose bit flag
				generalPurposeBitFlag: getUint16LE(buffer, 8),
				// 10 - Compression method
				compressionMethod: getUint16LE(buffer, 10),
				// 12 - File last modification time
				lastModFileTime: getUint16LE(buffer, 12),
				// 14 - File last modification date
				lastModFileDate: getUint16LE(buffer, 14),
				// 16 - CRC-32
				crc32: getUint32LE(buffer, 16),
				// 20 - Compressed size
				compressedSize: getUint32LE(buffer, 20),
				// 24 - Uncompressed size
				uncompressedSize: getUint32LE(buffer, 24),
				// 28 - File name length (n)
				fileNameLength: getUint16LE(buffer, 28),
				// 30 - Extra field length (m)
				extraFieldLength: getUint16LE(buffer, 30),
				// 32 - File comment length (k)
				fileCommentLength: getUint16LE(buffer, 32),
				// 34 - Disk number where file starts
				// 36 - Internal file attributes
				internalFileAttributes: getUint16LE(buffer, 36),
				// 38 - External file attributes
				externalFileAttributes: getUint32LE(buffer, 38),
				// 42 - Relative offset of local file header
				relativeOffsetOfLocalHeader: getUint32LE(buffer, 42),
			};

			if (rawEntry.generalPurposeBitFlag & 0x40) {
				throw new Error('strong encryption is not supported');
			}

			readEntryCursor += 46;

			const data = allEntriesBuffer.subarray(readEntryCursor, readEntryCursor + rawEntry.fileNameLength + rawEntry.extraFieldLength + rawEntry.fileCommentLength);
			rawEntry.nameBytes = data.slice(0, rawEntry.fileNameLength);
			rawEntry.name = decodeBuffer(rawEntry.nameBytes);

			// 46+n - Extra field
			const fileCommentStart = rawEntry.fileNameLength + rawEntry.extraFieldLength;
			const extraFieldBuffer = data.slice(rawEntry.fileNameLength, fileCommentStart);
			rawEntry.extraFields = [];
			let i = 0;
			while (i < extraFieldBuffer.length - 3) {
				const headerId = getUint16LE(extraFieldBuffer, i + 0);
				const dataSize = getUint16LE(extraFieldBuffer, i + 2);
				const dataStart = i + 4;
				const dataEnd = dataStart + dataSize;
				if (dataEnd > extraFieldBuffer.length) {
					throw new Error('extra field length exceeds extra field buffer size');
				}
				rawEntry.extraFields.push({
					id: headerId,
					data: extraFieldBuffer.slice(dataStart, dataEnd),
				});
				i = dataEnd;
			}

			// 46+n+m - File comment
			rawEntry.commentBytes = data.slice(fileCommentStart, fileCommentStart + rawEntry.fileCommentLength);
			rawEntry.comment = decodeBuffer(rawEntry.commentBytes);

			readEntryCursor += data.length;

			if (rawEntry.uncompressedSize            === 0xffffffff ||
					rawEntry.compressedSize              === 0xffffffff ||
					rawEntry.relativeOffsetOfLocalHeader === 0xffffffff) {
				// ZIP64 format
				// find the Zip64 Extended Information Extra Field
				const zip64ExtraField = rawEntry.extraFields.find(e => e.id === 0x0001);
				if (!zip64ExtraField) {
					return new Error('expected zip64 extended information extra field');
				}
				const zip64EiefBuffer = zip64ExtraField.data;
				let index = 0;
				// 0 - Original Size          8 bytes
				if (rawEntry.uncompressedSize === 0xffffffff) {
					if (index + 8 > zip64EiefBuffer.length) {
						throw new Error('zip64 extended information extra field does not include uncompressed size');
					}
					rawEntry.uncompressedSize = getUint64LE(zip64EiefBuffer, index);
					index += 8;
				}
				// 8 - Compressed Size        8 bytes
				if (rawEntry.compressedSize === 0xffffffff) {
					if (index + 8 > zip64EiefBuffer.length) {
						throw new Error('zip64 extended information extra field does not include compressed size');
					}
					rawEntry.compressedSize = getUint64LE(zip64EiefBuffer, index);
					index += 8;
				}
				// 16 - Relative Header Offset 8 bytes
				if (rawEntry.relativeOffsetOfLocalHeader === 0xffffffff) {
					if (index + 8 > zip64EiefBuffer.length) {
						throw new Error('zip64 extended information extra field does not include relative header offset');
					}
					rawEntry.relativeOffsetOfLocalHeader = getUint64LE(zip64EiefBuffer, index);
					index += 8;
				}
				// 24 - Disk Start Number      4 bytes
			}

			// check for Info-ZIP Unicode Path Extra Field (0x7075)
			// see https://github.com/thejoshwolfe/yauzl/issues/33
			const nameField = rawEntry.extraFields.find(e =>
					e.id === 0x7075 &&
					e.data.length >= 6 && // too short to be meaningful
					e.data[0] === 1 &&    // Version       1 byte      version of this extra field, currently 1
					getUint32LE(e.data, 1), crc$1.unsigned(rawEntry.nameBytes)); // NameCRC32     4 bytes     File Name Field CRC32 Checksum
																																		 // > If the CRC check fails, this UTF-8 Path Extra Field should be
																																		 // > ignored and the File Name field in the header should be used instead.
			if (nameField) {
					// UnicodeName Variable UTF-8 version of the entry File Name
					rawEntry.fileName = decodeBuffer(nameField.data.slice(5));
			}

			// validate file size
			if (rawEntry.compressionMethod === 0) {
				let expectedCompressedSize = rawEntry.uncompressedSize;
				if ((rawEntry.generalPurposeBitFlag & 0x1) !== 0) {
					// traditional encryption prefixes the file data with a header
					expectedCompressedSize += 12;
				}
				if (rawEntry.compressedSize !== expectedCompressedSize) {
					throw new Error(`compressed size mismatch for stored file: ${rawEntry.compressedSize} != ${expectedCompressedSize}`);
				}
			}
			rawEntries.push(rawEntry);
		}
		const zip = {
			comment,
			commentBytes,
		};
		return {
			zip,
			entries: rawEntries.map(e => new ZipEntry(reader, e)),
		};
	}

	async function readEntryDataHeader(reader, rawEntry) {
		if (rawEntry.generalPurposeBitFlag & 0x1) {
			throw new Error('encrypted entries not supported');
		}
		const buffer = await readAs(reader, rawEntry.relativeOffsetOfLocalHeader, 30);
		// note: maybe this should be passed in or cached on entry
		// as it's async so there will be at least one tick (not sure about that)
		const totalLength = await reader.getLength();

		// 0 - Local file header signature = 0x04034b50
		const signature = getUint32LE(buffer, 0);
		if (signature !== 0x04034b50) {
			throw new Error(`invalid local file header signature: 0x${signature.toString(16)}`);
		}

		// all this should be redundant
		// 4 - Version needed to extract (minimum)
		// 6 - General purpose bit flag
		// 8 - Compression method
		// 10 - File last modification time
		// 12 - File last modification date
		// 14 - CRC-32
		// 18 - Compressed size
		// 22 - Uncompressed size
		// 26 - File name length (n)
		const fileNameLength = getUint16LE(buffer, 26);
		// 28 - Extra field length (m)
		const extraFieldLength = getUint16LE(buffer, 28);
		// 30 - File name
		// 30+n - Extra field
		const localFileHeaderEnd = rawEntry.relativeOffsetOfLocalHeader + buffer.length + fileNameLength + extraFieldLength;
		let decompress;
		if (rawEntry.compressionMethod === 0) {
			// 0 - The file is stored (no compression)
			decompress = false;
		} else if (rawEntry.compressionMethod === 8) {
			// 8 - The file is Deflated
			decompress = true;
		} else {
			throw new Error(`unsupported compression method: ${rawEntry.compressionMethod}`);
		}
		const fileDataStart = localFileHeaderEnd;
		const fileDataEnd = fileDataStart + rawEntry.compressedSize;
		if (rawEntry.compressedSize !== 0) {
			// bounds check now, because the read streams will probably not complain loud enough.
			// since we're dealing with an unsigned offset plus an unsigned size,
			// we only have 1 thing to check for.
			if (fileDataEnd > totalLength) {
				throw new Error(`file data overflows file bounds: ${fileDataStart} +  ${rawEntry.compressedSize}  > ${totalLength}`);
			}
		}
		return {
			decompress,
			fileDataStart,
		};
	}

	async function readEntryDataAsBlob(reader, rawEntry, type) {
		const {decompress, fileDataStart} = await readEntryDataHeader(reader, rawEntry);
		if (!decompress) {
			const typedArrayOrBlob = await readAsBlobOrTypedArray(reader, fileDataStart, rawEntry.compressedSize, type);
			if (isBlob(typedArrayOrBlob)) {
				return typedArrayOrBlob;
			}
			return new Blob([isSharedArrayBuffer(typedArrayOrBlob.buffer) ? new Uint8Array(typedArrayOrBlob) : typedArrayOrBlob], {type});
		}
		throw "Compressed archives cannot be read";
	}

	async function unzipRaw(source) {
		let reader;
		if (typeof Blob !== 'undefined' && source instanceof Blob) {
			reader = new BlobReader(source);
		} else {
			throw new Error('unsupported source type');
		}

		const totalLength = await reader.getLength();

		if (totalLength > Number.MAX_SAFE_INTEGER) {
			throw new Error(`file too large. size: ${totalLength}. Only file sizes up 4503599627370496 bytes are supported`);
		}

		return await findEndOfCentralDirector(reader, totalLength);
	}

	// If the names are not utf8 you should use unzipitRaw
	window.unzipit = async function (source) {
		const {zip, entries} = await unzipRaw(source);
		return {
			zip,
			entries: Object.fromEntries(entries.map(v => [v.name, v])),
		};
	}
}());